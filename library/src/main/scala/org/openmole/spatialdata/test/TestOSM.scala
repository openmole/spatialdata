package org.openmole.spatialdata.test

import org.openmole.spatialdata.grid
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.real.{OSMGridGenerator, OSMGridSampling}
import org.openmole.spatialdata.utils.visualization

import scala.util.Random

object TestOSM {

  implicit val rng = new Random

  def testOSMGridSampling(): Unit = {

    //val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",100,200,50)
    val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",2,500,50)

    for (grid <- grids) {
      //println(Grid.gridToString(grid)+"\n\n")
      println(GridMorphology(grid._2))
    }

    //println(SpatialSampling.samplePointsInLayer("data/cities_europe.shp",10))

  }


  def testBuildingExtractor(): Unit = {

    // -4.247058 48.45855
    //
    val lon = 14.865577//-4.247058 //4.215393//2.3396859//2.3646
    val lat = 37.64593 //48.45855 //51.95148//48.8552569 //48.8295
    val shift = 500 // in meters

    //


    val g = OSMGridGenerator(lon,lat,shift,50).generateGrid
    //println(grid.gridToString(g))
    //println(GridMorphology(g))
    visualization.staticRasterVisualization(g)

    /*
    BuildingExtractor.getBuildingIntersection(48.82864, 2.36238, 48.83040, 2.36752).foreach(println)

    //48.82864, 2.36238, 48.83040, 2.36752
    val (x, y) = BuildingExtractor.WGS84ToPseudoMercator(lon, lat)
    val (west, south) = BuildingExtractor.PseudoMercatorToWGS84Mercator(x - shift, y - shift)
    val (east, north) = BuildingExtractor.PseudoMercatorToWGS84Mercator(x + shift, y + shift)
    val g = BuildingExtractor.getNegativeBuildingIntersection(south, west, north, east)
    println(g)
    if (g.isInstanceOf[MultiPolygon]) println(g.asInstanceOf[MultiPolygon].getNumGeometries)
    */

  }


}
