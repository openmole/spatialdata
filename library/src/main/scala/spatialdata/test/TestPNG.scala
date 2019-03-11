package spatialdata.test

import better.files.File
import spatialdata.synthetic.grid.{BlocksGridGenerator, ExpMixtureGenerator, PercolationGridGenerator, RandomGridGenerator}
import spatialdata.utils.io.PNG
import spatialdata._
import spatialdata.grid.GridGeneratorLauncher

import scala.util.Random

object TestPNG {

  def testRandomPNG(): Unit = {

    implicit val rng = new Random
    val launchers = Seq("random","expMixture","blocks","percolation").map{
      GridGeneratorLauncher(_,50,0.5,5,10.0,0.5,5,10,15,0.2,20,5.0)
    }

    launchers.foreach{
      case g => {
        val grid = g.getGrid
        PNG.write(grid, File(s"${g.generatorType}.png"))
      }
    }
  }
}
