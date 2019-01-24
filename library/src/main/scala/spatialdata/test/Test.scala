package spatialdata.test

import com.vividsolutions.jts.geom.MultiPolygon
import spatialdata.osm.BuildingExtractor


object Test extends App {

//  TestSynthetic.testRandomGrids()
//  BuildingExtractor.getBuildingIntersection(48.82864, 2.36238, 48.83040, 2.36752).foreach(println)
  val g = BuildingExtractor.getNegativeBuildingIntersection(48.82864, 2.36238, 48.83040, 2.36752)
  println(g)
  println(g.asInstanceOf[MultiPolygon].getNumGeometries)
}