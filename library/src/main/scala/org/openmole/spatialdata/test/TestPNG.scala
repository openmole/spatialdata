package org.openmole.spatialdata.test

import better.files.File
import org.openmole.spatialdata.grid.GridGeneratorLauncher
import org.openmole.spatialdata.grid.real.OSMGridSampling
import org.openmole.spatialdata.utils.io.PNG

import scala.util.Random

object TestPNG {

  def testPNG(): Unit = {

    implicit val rng = new Random(42L)
    val launchers = Seq("random","expMixture","blocks","percolation").map{
      GridGeneratorLauncher(_,200,
        0.5,
        200,10.0,0.5,
        80,10,30,
        0.2,20,4.0)
    }

    val directory = File("data") / "test"
    directory.createDirectories()
    launchers.foreach{
      case g => {
        val grid = g.getGrid
        PNG.write(grid, directory / s"${g.generatorType}.png")
      }
    }
  }
  def testOSMGridSampling(): Unit = {
    implicit val rng: Random = new Random

    val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",100,500,50)

    val directory = File("data") / "test"
    directory.createDirectories()

    for (agrid <- grids) {
      if (agrid._2.map(_.max).max > 0) {
//        println(grid.gridToString(agrid._2))
        PNG.write(agrid._2, directory / s"osm_${agrid._1}.png")
      }
    }
  }
}
