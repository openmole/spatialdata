package org.openmole.spatialdata.utils.osm

import java.io.{File, FileReader}
import java.sql.Connection
import java.util.Locale
import org.locationtech.jts.geom._
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.database.{MongoConnection, PostgisConnection}
import org.openmole.spatialdata.utils.gis.GISUtils.WGS84toPseudoMercatorFilter
import org.openmole.spatialdata.utils.gis.PoligonizerUtils
import org.openmole.spatialdata.utils.osm.OSMObject.{Node, Way}
import org.openmole.spatialdata.vector.{Attributes, Points}

import scala.util.Try

object APIExtractor {

  /**
    * Mode to retrieve OSM data
    *
    * ! need to rewrite archi to be generic enough for API/file (bbox not needed in case of file)
    */
  sealed trait OSMAPIMode
  case object OSMOverpass extends OSMAPIMode
  case object OSMDirect extends OSMAPIMode
  case class Postgresql(port: Int = 5433) extends OSMAPIMode
  case class Mongo(port: Int = 27017) extends OSMAPIMode
  case class OSMFile(file: String) extends OSMAPIMode
  case class OSMPBFFile(file: String) extends OSMAPIMode

  /**
    * Methods to extract buildings
    *
    *  ! some OSM polygons seem to be bad formed (failure "Way expected to be a polygon" for large sampling) => wrap in Try
    */
  object Buildings {

    def asPolygonSeq(ways: Seq[Way]): Seq[Polygon] = {
      val fact = new JTSGeometryFactory()
      def toPolygon(way: OSMObject.Way): Option[Polygon] =
        if (way.getTag("building") != null) Try(fact.createPolygon(way)).toOption else None
      ways.flatMap(toPolygon)
    }


    /**
      * Get buildings from OSM
      *
      * !simplify is not used ? + which precision if simplify? (arbitrary)
      *
      * @param south south coord
      * @param west west coord
      * @param north north coord
      * @param east east coord
      * @param mode osm,overpass, postgresql
      * @return
      */
    def getBuildings(south: Double = 0.0,
                     west: Double = 0.0,
                     north: Double = 0.0,
                     east: Double = 0.0,
                     mode: OSMAPIMode = OSMOverpass,
                     attributes: Seq[String] = Seq.empty[String]
                    ): (Seq[Polygon], Seq[Attributes]) = {
      Locale.setDefault(Locale.ENGLISH)
      mode match {
        case OSMOverpass =>
          val overpass = new APIOverpass
          val root = overpass.get(south, west, north, east, hasKeyValue=("building",Seq("yes")))
          utils.log("retrieved via overpass " + east + " n=" + north + " s=" + south + "w=" + west)
          val ways = root.getWays
          (asPolygonSeq(ways), ways.map(w => attributes.map(a => (a,w.getAttribute(a))).toMap))

        case OSMDirect =>
          val api = new APIConnection()
          val root = api.get(south, west, north, east)
          utils.log("retrieved via standard api " + east + " n=" + north + " s=" + south + "w=" + west)
          val ways = root.getWays
          (asPolygonSeq(ways), ways.map(w => attributes.map(a => (a,w.getAttribute(a))).toMap))

        case Postgresql(port) =>
          implicit val connection: Connection = PostgisConnection.initPostgis(database ="buildings",port = port)
          val polygons = PostgisConnection.bboxRequest(west,south,east,north,"ways")
          utils.log("retrieved via postgresql " + east + " n=" + north + " s=" + south + "w=" + west+" : "+polygons.size+" buildings")
          PostgisConnection.closeConnection()
          (polygons, Seq.fill(polygons.length)(Attributes.empty))

        case Mongo(port) =>
          MongoConnection.initMongo(database = "buildings",port=port)
          val polygons = MongoConnection.bboxRequest(west,south,east,north,"buildings")
          utils.log("retrieved via mongo " + east + " n=" + north + " s=" + south + "w=" + west+" : "+polygons.size+" buildings")
          MongoConnection.closeMongo()
          (polygons, Seq.fill(polygons.length)(Attributes.empty))

        case OSMFile(file) =>
          val root = new OSMRoot
          OSMXmlParser(root).parse(new FileReader(new File(file)))
          val ways = root.getWays
          (asPolygonSeq(ways), ways.map(w => attributes.map(a => (a,w.getAttribute(a))).toMap))

        case OSMPBFFile(file) =>
          val fact = new GeometryFactory()
          val (_,ways) = utils.osm.OSMPBFFile.readPBFFile(file)
          (ways.lines.flatMap{l =>
            val coords = l.getCoordinates
            if (coords.length<=3) None else {
              val closedcoords = if (coords(0).equals2D(coords.last)) coords else coords ++ Array(coords(0)) // force closing the linestring - should do the same in geom factory
              Some(fact.createPolygon(closedcoords))
            }
          }, Seq.fill(ways.lines.length)(Attributes.empty))
      }
      /*

      def simplify(polygon: Polygon) = {
        GeometryPrecisionReducer.reduce(polygon, new PrecisionModel(10000)) match {
          case p: Polygon => Seq(p)
          case mp: MultiPolygon => for (i <- 0 until mp.getNumGeometries) yield mp.getGeometryN(i).asInstanceOf[Polygon]
          case _ => Seq()
        }
      }*/
      //.flatMap(simplify)
    }

    def getBuildingIntersection(south: Double, west: Double, north: Double, east: Double, mode: OSMAPIMode = OSMOverpass): Seq[Geometry] = {
      val (buildings, _) = getBuildings(south, west, north, east, mode)
      val fact = new GeometryFactory()
      val env = fact.createPolygon(fact.createLinearRing(Array(new Coordinate(west, north), new Coordinate(east, north), new Coordinate(east, south), new Coordinate(west, south), new Coordinate(west, north))), Array())
      //buildings.map(_.intersection(env))
      (buildings :+ env).foreach(_.apply(new WGS84toPseudoMercatorFilter))
      PoligonizerUtils.getPolygonIntersection(buildings, env)
    }


    def getNegativeBuildingIntersection(south: Double, west: Double, north: Double, east: Double, mode: OSMAPIMode = OSMOverpass): Geometry = {
      val (buildings, _) = getBuildings(south, west, north, east, mode)
      val fact = new GeometryFactory()
      val env = fact.createPolygon(fact.createLinearRing(Array(new Coordinate(west, north), new Coordinate(east, north), new Coordinate(east, south), new Coordinate(west, south), new Coordinate(west, north))), Array())
//      var res = Try {
//        val union = fact.createMultiPolygon(buildings.toArray).union()
//        var result = scala.collection.mutable.Buffer[Polygon]()
//        for (i <- 0 until union.getNumGeometries) result += fact.createPolygon(fact.createLinearRing(union.getGeometryN(i).asInstanceOf[Polygon].getExteriorRing.getCoordinateSequence), Array())
//        env.difference(fact.createMultiPolygon(result.toArray).union)
//      }
//      if (res.isSuccess) {
//        res.get.apply(new WGS84toPseudoMercatorFilter)
//        res.get
//      }else{
//        env
//      }
      (buildings :+ env).foreach(_.apply(new WGS84toPseudoMercatorFilter))
      val res = fact.createMultiPolygon(PoligonizerUtils.getPolygonDifference(buildings, env).toArray)
      res
    }
  }


  /**
    * Methods to extract highways
    */
  object Highways {


    /**
      * Convert API result to a sequence of LineString
      * @param ways ways
      * @param tags tags to keep
      * @return
      */
    def asLineStringSeq(ways: Seq[Way], tags: Map[String,Seq[String]]): Seq[LineString] = {
      val fact = new JTSGeometryFactory()
      def toLineString(way: Way): Option[LineString] = {
        val validway = tags.toSeq.map{
          case (tag,values) =>
            val waytag = way.getTag(tag)
            if(tag == null) false
            else {
              values.contains(waytag)
            }
        }.reduce(_&_)
        if (validway) Try(fact.createLineString(way)).toOption else None
      }
      ways.flatMap(toLineString)
    }

    /**
      * Get highways from API
      *
      * ! should return Lines with specified attributes
      * ! the has-kv with | does not work for highway -> yes with regv, see https://wiki.openstreetmap.org/wiki/Overpass_API/Language_Guide#The_Overpass_API_languages
      *
      * @param south south coord
      * @param west west coord
      * @param north north coord
      * @param east east coord
      * @param tags tags - used only with overpass query
      * @return
      *
      */
    def getHighways(south: Double, west: Double, north: Double, east: Double,
                    tags: Map[String,Seq[String]],
                    mode: OSMAPIMode = OSMOverpass): Seq[LineString] = {
      Locale.setDefault(Locale.ENGLISH)
      mode match {
        case OSMOverpass =>
          val overpass = new APIOverpass
          // if only one tag requested, use as a filter in the overpass request
          val root = overpass.get(south, west, north, east,
            hasKeyValue=if (tags.size==1) tags.toSeq.head else ("",Seq(""))
          )
          val res = asLineStringSeq(root.getWays,tags)
          //utils.log("Highways from overpass " +res)
          res

        case OSMDirect =>
          val api = new APIConnection()
          val root = api.get(south, west, north, east)
          val res = asLineStringSeq(root.getWays,tags)
          //utils.log("Highways from OSM (API): "+res)
          res

        case _ => Seq.empty
      }
    }

  }


  object OSMPoints {

    /**
      * OSM Node enumerator to Points
      * @param nodes nodes
      * @param tags tags
      * @return
      */
    def asPoints(nodes: Seq[Node], tags: Map[String,Seq[String]]): Points = {
      val fact = new JTSGeometryFactory()
      def toPoint(node: Node): Option[Point] = {
        val validnode = tags.toSeq.map{
          case (tag,values) =>
            val nodetag = node.getTag(tag)
            if(tag == null) false
            else {
              values.contains(nodetag)
            }
        }.reduce(_&_)
        if (validnode) Try(fact.createPoint(node)).toOption else None
      }
      Points(nodes.flatMap(toPoint), Seq.empty)
    }


    /**
      * Get points from API
      *
      * ! the has-kv with | does not work for highway
      *
      * @param south south coord
      * @param west west coord
      * @param north north coord
      * @param east east coord
      * @param tags tags
      * @param mode mode
      * @return
      */
    def getPoints(south: Double, west: Double, north: Double, east: Double,
                  tags: Map[String,Seq[String]],
                  mode: OSMAPIMode = OSMOverpass
                 ): Points = {
      Locale.setDefault(Locale.ENGLISH)
      mode match {
        case OSMOverpass =>
          val overpass = new APIOverpass
          // if only one tag requested, use as a filter in the overpass request
          val root = overpass.get(south, west, north, east,
            hasKeyValue=("",Seq("")) //if (tags.size==1) tags.toSeq(0) else ("",Seq(""))
          )
          val res = asPoints(root.getNodes,tags)
          utils.log("Nodes from overpass " +res)
          res

        case OSMDirect =>
          val api = new APIConnection()
          val root = api.get(south, west, north, east)
          val res = asPoints(root.getNodes,tags)
          utils.log("Nodes from OSM (API): "+res)
          res

        case _ => org.openmole.spatialdata.vector.Points.empty
      }
    }


  }


}
