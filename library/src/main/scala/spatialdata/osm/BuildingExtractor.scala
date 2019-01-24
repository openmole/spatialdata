package spatialdata.osm

import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory, Polygon}
import se.kodapan.osm.domain.Way
import se.kodapan.osm.domain.root.Root.Enumerator
import se.kodapan.osm.jts.JtsGeometryFactory
import se.kodapan.osm.services.api.v_0_6.ApiConnection

import scala.collection.JavaConverters

object BuildingExtractor {
  def asPolygonSeq(e: Enumerator[Way]) = {
    var result = scala.collection.mutable.Buffer[Polygon]()
    val fact = new JtsGeometryFactory()
    var way = e.next()
    while (way != null) {
      val building = way.getTag("building")
      if (building != null /* && building.equals("yes")*/ ) result += fact.createPolygon(way)
      way = e.next()
    }
    Seq(result:_*)
  }
  def getBuildings(south: Double, west: Double, north: Double, east: Double) = {
    val api = new ApiConnection()
    val root = api.get(south, west, north, east)
    val login = sys.env("OSM_LOGIN")
    val password = sys.env("OSM_PASSWORD")
    api.authenticate(login, password)
    asPolygonSeq(root.enumerateWays())
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
    env.difference(fact.createMultiPolygon(result.toArray).union)
  }
}
