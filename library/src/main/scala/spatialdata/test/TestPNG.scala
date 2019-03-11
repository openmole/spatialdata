package spatialdata.test

import better.files.File
import spatialdata.utils.io.PNG
import spatialdata.grid.GridGeneratorLauncher

import scala.util.Random

object TestPNG {

  def testPNG(): Unit = {

    implicit val rng = new Random
    val launchers = Seq("random","expMixture","blocks","percolation").map{
      GridGeneratorLauncher(_,50,0.5,5,10.0,0.5,5,10,15,0.1,20,2.0)
    }

    launchers.foreach{
      case g => {
        val grid = g.getGrid
        PNG.write(grid, File(s"${g.generatorType}.png"))
      }
    }
  }
}
