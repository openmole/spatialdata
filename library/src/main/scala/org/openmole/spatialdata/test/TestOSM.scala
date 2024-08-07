package org.openmole.spatialdata.test

import org.locationtech.jts.io.WKTWriter
import org.openmole.spatialdata.grid
import org.openmole.spatialdata.grid.real.{OSMGridGenerator, OSMGridSampling}
import org.openmole.spatialdata.utils.osm.{APIExtractor, APIOverpass, OSMRoot, OSMXmlParser}
import org.openmole.spatialdata.utils.osm.APIExtractor.Buildings.asPolygonSeq
import org.openmole.spatialdata.utils.osm.APIExtractor.{OSMDirect, OSMPBFFile}
import org.openmole.spatialdata.vector.real.OSMBuildingsGenerator
//import org.openmole.spatialdata.utils.visualization
//import org.openmole.spatialdata.grid.measures.GridMorphology

import scala.util.Random

object TestOSM {

  implicit val rng: Random = new Random


  def testOSMBuildingsGenerator(): Unit = {
    val lon = 2.424431
    val lat = 48.845189
    val window = 200 // in meters

    val g = OSMBuildingsGenerator(lon,lat,window, attributes = Seq("height", "id")).generatePolygons
    //println(g)
    println(g.polygons.map(_.getCentroid))
    println(g.attributes) // null height: keys not retrieved at OSM object construction?
  }

  def testOSMPBFFile(): Unit = {
    val (buildings, _) = APIExtractor.Buildings.getBuildings(mode = OSMPBFFile(//"http://download.geofabrik.de/europe/andorra-latest.osm.pbf"
        System.getenv("CS_HOME_EXT2")+"/Data/OSM/Geofabrik/andorra/andorra_buildings_20201117.osm.pbf"
     ))
    val wktwriter: WKTWriter = new WKTWriter()
    println(buildings.map(wktwriter.write(_)).mkString("\n"))
    println("Buildings: "+buildings.size)
  }


  def testOverpass(): Unit = {

    import java.io.StringReader

    val overpass = new APIOverpass

    val root = new OSMRoot
    val parser = OSMXmlParser(root)

    parser.parse(new StringReader(overpass.execute(
      """
        |<union>
        |  <bbox-query s="51.249" w="7.148" n="51.251" e="7.152"/>
        |  <recurse type="up"/>
        |</union>
        |<print mode="meta"/>
      """.stripMargin)))
    asPolygonSeq(root.getWays).foreach(println)
  }




  def testOSMGridSampling(): Unit = {

    //val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",100,200,50)
    val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",2,500,50, mode = OSMDirect)

    for (grid <- grids) {
      println(org.openmole.spatialdata.grid.gridToString(grid._2)+"\n\n")
      //println(GridMorphology(grid._2))
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
    println(grid.gridToString(g))
    //println(GridMorphology(g))
    //visualization.staticRasterVisualization(g)

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
