package spatialdata.osm

import java.io.StringReader
import java.util.Locale

import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.precision.GeometryPrecisionReducer
import se.kodapan.osm.domain.Way
import se.kodapan.osm.domain.root.PojoRoot
import se.kodapan.osm.domain.root.Root.Enumerator
import se.kodapan.osm.jts.JtsGeometryFactory
import se.kodapan.osm.parser.xml.instantiated.InstantiatedOsmXmlParser
import se.kodapan.osm.services.api.v_0_6.ApiConnection
import se.kodapan.osm.services.overpass.Overpass
import spatialdata.utils.gis.GISUtils._

import scala.collection.JavaConverters
import scala.util.Try

object APIExtractor {

  object Buildings {

    def asPolygonSeq(e: Enumerator[Way]) = {
      var result = scala.collection.mutable.Buffer[Polygon]()
      val fact = new JtsGeometryFactory()
      var way: Way = e.next
      while (way != null) {
        val building = way.getTag("building")
        if (building != null /* && building.equals("yes")*/ ) {
          // FIXME some OSM polygons seem to be bad formed (failure "Way expected to be a polygon" for large sampling) => wrap in Try
          val potentialPolygon = Try(fact.createPolygon(way))
          if (potentialPolygon.isSuccess) {
            result += potentialPolygon.get
          }
        }
        way = e.next
      }
      Seq(result: _*)
    }

    def getBuildings(south: Double, west: Double, north: Double, east: Double, useOverpass: Boolean = true): Seq[Polygon] = {
      Locale.setDefault(Locale.ENGLISH)
      val root = if (useOverpass) {
        val overpass = new Overpass
        overpass.setUserAgent("Spatial Data extraction")
        overpass.open()
        val root = new PojoRoot
        val parser = InstantiatedOsmXmlParser.newInstance
        parser.setRoot(root)
        parser.parse(new StringReader(overpass.execute(
          s"""
            |<union>
            |  <bbox-query s="$south" w="$west" n="$north" e="$east"/>
            |  <recurse type="up"/>
            |</union>
            |<print mode="meta"/>
          """.stripMargin)))
        root
      } else {
        val api = new ApiConnection()
        api.get(south, west, north, east)
      }
      def simplify(polygon: Polygon) = {
        val g = GeometryPrecisionReducer.reduce(polygon, new PrecisionModel(10000)) //FIXME: This is arbitrary
        if (g.isInstanceOf[Polygon]) Seq(g.asInstanceOf[Polygon])
        else if (g.isInstanceOf[MultiPolygon]) {
          val mp = g.asInstanceOf[MultiPolygon]
          (for (i <- 0 until mp.getNumGeometries) yield mp.getGeometryN(i).asInstanceOf[Polygon]).toSeq
        } else Seq()
      }
      asPolygonSeq(root.enumerateWays).flatMap(simplify(_))
    }

    def getBuildingIntersection(south: Double, west: Double, north: Double, east: Double, useOverpass: Boolean = true): Seq[Geometry] = {
      val buildings = getBuildings(south, west, north, east, useOverpass)
      val fact = new GeometryFactory()
      val env = fact.createPolygon(fact.createLinearRing(Array(new Coordinate(west, north), new Coordinate(east, north), new Coordinate(east, south), new Coordinate(west, south), new Coordinate(west, north))), Array())
      buildings.map(_.intersection(env))
    }


    def getNegativeBuildingIntersection(south: Double, west: Double, north: Double, east: Double, useOverpass: Boolean = true): Geometry = {
      val buildings = getBuildings(south, west, north, east, useOverpass)
      val fact = new GeometryFactory()
      val env = fact.createPolygon(fact.createLinearRing(Array(new Coordinate(west, north), new Coordinate(east, north), new Coordinate(east, south), new Coordinate(west, south), new Coordinate(west, north))), Array())

      var res = Try {
        val union = fact.createMultiPolygon(buildings.toArray).union()
        var result = scala.collection.mutable.Buffer[Polygon]()
        for (i <- 0 until union.getNumGeometries) result += fact.createPolygon(fact.createLinearRing(union.getGeometryN(i).asInstanceOf[Polygon].getExteriorRing.getCoordinateSequence), Array())
        env.difference(fact.createMultiPolygon(result.toArray).union)
      }
      if (res.isSuccess) {
        res.get.apply(new WGS84toPseudoMercatorFilter)
        res.get
      }else{
        env
      }
    }

  }


  object Highways {


    def asLineStringSeq(e: Enumerator[Way], tags: Map[String,Seq[String]]): Seq[LineString] = {
      var result = scala.collection.mutable.Buffer[LineString]()
      val fact = new JtsGeometryFactory()
      var way: Way = e.next
      while (way != null) {
        val validway = tags.map{
          case (tag,values) => {
            val waytag = way.getTag(tag)
            if(tag == null) false
            else {
              values.contains(waytag)
            }
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
      Seq(result: _*)
    }

    def getHighways(south: Double, west: Double, north: Double, east: Double,tags: Map[String,Seq[String]]): Seq[LineString] = {
      Locale.setDefault(Locale.ENGLISH)
      val api = new ApiConnection()
      val root = api.get(south, west, north, east)
      asLineStringSeq(root.enumerateWays,tags)
    }
  }
}
