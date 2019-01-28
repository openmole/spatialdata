package spatialdata.test

import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory, MultiPolygon}
import spatialdata.osm.{BuildingExtractor, OSMGridGenerator}
import spatialdata.grid.Grid
import spatialdata.measures.Morphology
import spatialdata.sampling.{OSMGridSampling, SpatialSampling}

import scala.util.Random

object Test extends App {

  implicit val rng: Random = new Random

  val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",100,200,50)

  for (grid <- grids) {
    println(Grid.gridToString(grid)+"\n\n")
    println(Morphology(grid))
  }

/*
  //println(SpatialSampling.samplePointsInLayer("data/cities_europe.shp",10))


  //TestIndicators.testMoran

//  TestSynthetic.testRandomGrids()
//  BuildingExtractor.getBuildingIntersection(48.82864, 2.36238, 48.83040, 2.36752).foreach(println)
  val lon = 2.3646
  val lat = 48.8295
  val shift = 100 // in meters

// println(Grid.gridToString(OSMGridGenerator(lon,lat,shift,50).generateGrid))


  //48.82864, 2.36238, 48.83040, 2.36752
  val (x, y) = BuildingExtractor.WGS84ToPseudoMercator(lon, lat)
  val (west, south) = BuildingExtractor.PseudoMercatorToWGS84Mercator(x - shift, y - shift)
  val (east, north) = BuildingExtractor.PseudoMercatorToWGS84Mercator(x + shift, y + shift)
  val g = BuildingExtractor.getNegativeBuildingIntersection(south, west, north, east)
  println(g)
<<<<<<< HEAD
  if (g.isInstanceOf[MultiPolygon]) println(asInstanceOf[MultiPolygon].getNumGeometries)
  */

}