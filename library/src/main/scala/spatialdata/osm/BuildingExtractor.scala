package spatialdata.osm

import java.util.Locale

import com.vividsolutions.jts.geom._
import se.kodapan.osm.domain.Way
import se.kodapan.osm.domain.root.Root.Enumerator
import se.kodapan.osm.jts.JtsGeometryFactory
import se.kodapan.osm.services.api.v_0_6.ApiConnection


import scala.collection.JavaConverters

object BuildingExtractor {
  /**
    * Convert WGS84 coordinates to EPSG:3857 - WGS 84 / Pseudo-Mercator -- Spherical Mercator.
    * @param lon longitude
    * @param lat latitude
    * @return Pseudo-Mercator coordinates
    */
  def WGS84ToPseudoMercator(lon: Double, lat: Double) = {
    val x = lon * 20037508.34 / 180
    val y = (Math.log(Math.tan((90 + lat) * Math.PI / 360)) / (Math.PI / 180) ) * 20037508.34 / 180
    (x, y)
  }
  /**
    * Convert EPSG:3857 - WGS 84 / Pseudo-Mercator -- Spherical Mercator coordinates to WGS84 - EPSG:4326.
    * @param x x
    * @param y y
    * @return WGS84 - EPSG:4326 (lon, lat)
    */
  def PseudoMercatorToWGS84Mercator(x: Double, y: Double) = {
    val lon = (x / 20037508.34) * 180
    val lat = (y / 20037508.34) * 180
    (lon, 180 / Math.PI * (2 * Math.atan(Math.exp(lat * Math.PI / 180)) - Math.PI / 2))
  }
  /**
    * CoordinateSequenceFilter to convert WGS84 coordinate sequences to WGS 84 / Pseudo-Mercator (EPSG:3857).
    */
  class WGS84toPseudoMercatorFilter extends CoordinateSequenceFilter {
    def filter(seq: CoordinateSequence, i: Int) = {
      if ((i != seq.size - 1) || (seq.getCoordinate(i) != seq.getCoordinate(0))) {
        val coord = seq.getCoordinate(i)
        val t = WGS84ToPseudoMercator(coord.x, coord.y)
        coord.setCoordinate(new Coordinate(t._1, t._2))
      }
    }
    def isDone = false
    def isGeometryChanged = true
  }
  def asPolygonSeq(e: Enumerator[Way]) = {
    var result = scala.collection.mutable.Buffer[Polygon]()
    val fact = new JtsGeometryFactory()
    var way: Way = e.next
    while (way != null) {
      val building = way.getTag("building")
      if (building != null /* && building.equals("yes")*/ ) result += fact.createPolygon(way)
      way = e.next
    }
    Seq(result:_*)
  }
  def getBuildings(south: Double, west: Double, north: Double, east: Double) = {
    Locale.setDefault(Locale.ENGLISH)
    val api = new ApiConnection()//"http://master.apis.dev.openstreetmap.org/api"
    val root = api.get(south, west, north, east)
//    val login = sys.env("OSM_LOGIN")
//    val password = sys.env("OSM_PASSWORD")
//    api.authenticate(login, password)
    asPolygonSeq(root.enumerateWays)
  }
  def getBuildingIntersection(south: Double, west: Double, north: Double, east: Double) = {
    val buildings = getBuildings(south, west, north, east)
    val fact = new GeometryFactory()
    val env = fact.createPolygon(fact.createLinearRing(Array(new Coordinate(west, north),new Coordinate(east, north),new Coordinate(east, south),new Coordinate(west, south),new Coordinate(west, north))),Array())
    buildings.map(_.intersection(env))
  }
  def getNegativeBuildingIntersection(south: Double, west: Double, north: Double, east: Double) = {
    val buildings = getBuildings(south, west, north, east)
    val fact = new GeometryFactory()
    val env = fact.createPolygon(fact.createLinearRing(Array(new Coordinate(west, north),new Coordinate(east, north),new Coordinate(east, south),new Coordinate(west, south),new Coordinate(west, north))),Array())
    val union = fact.createMultiPolygon(buildings.toArray).union()
    var result = scala.collection.mutable.Buffer[Polygon]()
    for (i <- 0 until union.getNumGeometries) result += fact.createPolygon(fact.createLinearRing(union.getGeometryN(i).asInstanceOf[Polygon].getExteriorRing.getCoordinateSequence),Array())
    var res = env.difference(fact.createMultiPolygon(result.toArray).union)
    res.apply(new WGS84toPseudoMercatorFilter)
    res
  }
}
