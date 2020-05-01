package org.openmole.spatialdata.utils.osm.api

import java.sql.Connection
import java.util.Locale

import org.locationtech.jts.geom._

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.database.{MongoConnection, PostgisConnection}
import org.openmole.spatialdata.utils.gis.GISUtils.WGS84toPseudoMercatorFilter
import org.openmole.spatialdata.utils.gis.PoligonizerUtils
import org.openmole.spatialdata.utils.osm.JtsGeometryFactory
import org.openmole.spatialdata.utils.osm._
import org.openmole.spatialdata.vector.Points

import scala.util.Try

object APIExtractor {

  sealed trait OSMAPIMode
  case object OSMOverpass extends OSMAPIMode
  case object OSMDirect extends OSMAPIMode
  case class Postgresql(port: Int = 5433) extends OSMAPIMode
  case class Mongo(port: Int = 27017) extends OSMAPIMode


  /**
    * Methods to extract buildings
    *
    *  ! some OSM polygons seem to be bad formed (failure "Way expected to be a polygon" for large sampling) => wrap in Try
    */
  object Buildings {

    def asPolygonSeq(e: Root.Enumerator[Way]): Seq[Polygon] = {
      var result = scala.collection.mutable.Buffer[Polygon]()
      val fact = new JtsGeometryFactory()
      var way: Way = e.next
      while (way != null) {
        val building = way.getTag("building")
        if (building != null /* && building.equals("yes")*/ ) {
          val potentialPolygon = Try(fact.createPolygon(way))
          if (potentialPolygon.isSuccess) {
            result += potentialPolygon.get
          }
        }
        way = e.next
      }
      result.toSeq
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
    def getBuildings(south: Double, west: Double, north: Double, east: Double, mode: OSMAPIMode = OSMOverpass): Seq[Polygon] = {
      Locale.setDefault(Locale.ENGLISH)
      mode match {
        case OSMOverpass =>
          val overpass = new Overpass
          val root = overpass.get(south, west, north, east, hasKeyValue=("building",Seq("yes")))
          utils.log("retrieved via overpass " + east + " n=" + north + " s=" + south + "w=" + west)
          asPolygonSeq(root.enumerateWays)

        case OSMDirect =>
          val api = new ApiConnection()
          val res = api.get(south, west, north, east)
          utils.log("retrieved via standard api " + east + " n=" + north + " s=" + south + "w=" + west)
          asPolygonSeq(res.enumerateWays)

        case Postgresql(port) =>
          implicit val connection: Connection = PostgisConnection.initPostgis(database ="buildings",port = port)
          val polygons = PostgisConnection.bboxRequest(west,south,east,north,"ways")
          utils.log("retrieved via postgresql " + east + " n=" + north + " s=" + south + "w=" + west+" : "+polygons.size+" buildings")
          PostgisConnection.closeConnection
          polygons

        case Mongo(port) =>
          MongoConnection.initMongo(database = "buildings",port=port)
          val polygons = MongoConnection.bboxRequest(west,south,east,north,"buildings")
          utils.log("retrieved via mongo " + east + " n=" + north + " s=" + south + "w=" + west+" : "+polygons.size+" buildings")
          MongoConnection.closeMongo()
          polygons

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
      val buildings = getBuildings(south, west, north, east, mode)
      val fact = new GeometryFactory()
      val env = fact.createPolygon(fact.createLinearRing(Array(new Coordinate(west, north), new Coordinate(east, north), new Coordinate(east, south), new Coordinate(west, south), new Coordinate(west, north))), Array())
      //buildings.map(_.intersection(env))
      (buildings :+ env).foreach(_.apply(new WGS84toPseudoMercatorFilter))
      PoligonizerUtils.getPolygonIntersection(buildings, env)
    }


    def getNegativeBuildingIntersection(south: Double, west: Double, north: Double, east: Double, mode: OSMAPIMode = OSMOverpass): Geometry = {
      val buildings = getBuildings(south, west, north, east, mode)
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
      * @param e OSM enumerator
      * @param tags tags to keep
      * @return
      */
    def asLineStringSeq(e: Root.Enumerator[Way], tags: Map[String,Seq[String]]): Seq[LineString] = {
      val result = scala.collection.mutable.Buffer[LineString]()
      val fact = new JtsGeometryFactory()
      var way: Way = e.next
      while (way != null) {
        val validway = tags.toSeq.map{
          case (tag,values) =>
            val waytag = way.getTag(tag)
            if(tag == null) false
            else {
              values.contains(waytag)
            }
        }.reduce(_&_)
        if (validway) {
          val potentialLine = Try(fact.createLineString(way))
          if (potentialLine.isSuccess) {
            result += potentialLine.get
          }
        }
        way = e.next
      }
      result.toSeq
    }

    /**
      * Get highways from API
      *
      * ! should return Lines with specified attributes
      * ! the has-kv with | does not work for highway
      *
      * @param south south coord
      * @param west west coord
      * @param north north coord
      * @param east east coord
      * @param tags tags
      * @return FIXME
      *
      */
    def getHighways(south: Double, west: Double, north: Double, east: Double,
                    tags: Map[String,Seq[String]],
                    mode: OSMAPIMode = OSMOverpass): Seq[LineString] = {
      Locale.setDefault(Locale.ENGLISH)
      mode match {
        case OSMOverpass =>
          val overpass = new Overpass
          // if only one tag requested, use as a filter in the overpass request
          val root = overpass.get(south, west, north, east,
            hasKeyValue=("",Seq("")) //if (tags.size==1) tags.toSeq(0) else ("",Seq(""))
          )
          val res = asLineStringSeq(root.enumerateWays,tags)
          utils.log("Highways from overpass " +res)
          res

        case OSMDirect =>
          val api = new ApiConnection()
          val root = api.get(south, west, north, east)
          val res = asLineStringSeq(root.enumerateWays,tags)
          utils.log("Highways from OSM (API): "+res)
          res

        case _ => Seq.empty
      }
    }

  }


  object Points {

    /**
      * OSM Node enumerator to Points
      * @param root enumerator
      * @param tags tags
      * @return
      */
    def asPoints(root: Root.Enumerator[Node], tags: Map[String,Seq[String]]): org.openmole.spatialdata.vector.Points = {
      val result = scala.collection.mutable.Buffer[Point]()
      val fact = new JtsGeometryFactory()
      var node: Node = root.next
      while (node != null) {
        val validnode = tags.toSeq.map{
          case (tag,values) =>
            val nodetag = node.getTag(tag)
            if(tag == null) false
            else {
              values.contains(nodetag)
            }
        }.reduce(_&_)
        if (validnode) {
          val potentialPoint = Try(fact.createPoint(node))
          if (potentialPoint.isSuccess) {
            result += potentialPoint.get
          }
        }
        node = root.next
      }
      org.openmole.spatialdata.vector.Points(result.toSeq, Map.empty)
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
          val overpass = new Overpass
          // if only one tag requested, use as a filter in the overpass request
          val root = overpass.get(south, west, north, east,
            hasKeyValue=("",Seq("")) //if (tags.size==1) tags.toSeq(0) else ("",Seq(""))
          )
          val res = asPoints(root.enumerateNodes,tags)
          utils.log("Nodes from overpass " +res)
          res

        case OSMDirect =>
          val api = new ApiConnection()
          val root = api.get(south, west, north, east)
          val res = asPoints(root.enumerateNodes,tags)
          utils.log("Nodes from OSM (API): "+res)
          res

        case _ => org.openmole.spatialdata.vector.Points.empty
      }
    }


  }


}
