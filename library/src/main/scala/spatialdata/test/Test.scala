package spatialdata.test

import spatialdata.osm.BuildingExtractor


object Test extends App {

//  TestSynthetic.testRandomGrids()
  val seq = BuildingExtractor.getBuildings(48.82864, 2.36238, 48.83040, 2.36752)
  println(seq.size)
  seq.foreach(println)
}