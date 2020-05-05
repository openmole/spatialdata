package org.openmole.spatialdata.test

import org.openmole.spatialdata
import org.openmole.spatialdata.application.urbanmorphology.GridGeneratorLauncher
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.synthetic.{BlocksGridGenerator, PercolationGridGenerator, RandomGridGenerator}

import scala.io.Source
import scala.util.Random

object TestSynthetic {

  def testGeneratorCalibration: Unit = {

    val pc1obj = 0.5
    val pc2obj = 0.0
    //val rotation = CSV.readMat("data/calib/pca.csv")
    //val norm = CSV.readMat("data/calib/norm.csv")
    val rotation = Source.fromFile("data/calib/pca.csv").getLines().toArray.map{_.split(",").map{_.toDouble}}
    val norm = Source.fromFile("data/calib/norm.csv").getLines().toArray.map{_.split(",").map{_.toDouble}}

    implicit val rng = new Random

    import org.openmole.spatialdata.application.urbanmorphology.GridGeneratorCalibration._

    def projection(morphology: GridMorphology): Array[Double] = GridMorphology.rotation(rotation,norm)(morphology)
    def objective(pcs: Array[Double]): Double = math.sqrt(math.pow(pcs(0)-pc1obj,2)+math.pow(pcs(1)-pc2obj,2))

    (1 to 20).foreach { case blocknum =>
      println("MSE on two first pcs = " + CalibrateBlocksGridGenerator(50, blocknum, 1, 10).calibrate(projection, objective))
    }

  }


  def testGeneratorLauncher(): Unit = {

    implicit val rng = new Random

    val launchers = Seq("random","expMixture","blocks","percolation").map{
      GridGeneratorLauncher(_,
        50,0.5,5,10.0,0.5,5,10,15,0.2,20,3.0
      )
    }

    launchers.foreach{case g => for(_ <- 0 until 10) {println(g.getMorphology)}}

  }


  def testPercolationGrid(): Unit = {
    implicit val rng = new Random
    val grid = PercolationGridGenerator(50,0.5,20,3,10000).generateGrid

    println(spatialdata.grid.gridToString(grid))
  }


  def testBlocksGrid(): Unit = {

    implicit val rng = new Random

    val grid = BlocksGridGenerator(Left(50),5,10,15).generateGrid

    println(spatialdata.grid.gridToString(grid))

  }



  /**
    * Test : stat distribution of Moran for random grids
    */
  def testRandomGrids(): Unit = {

    implicit val rng: Random = new Random

    val size = 25

    // test empirical distrib of moran for random grids

    val morans = (1 until 10000).map{i =>
      if (i%100==0){println(i)}
      GridMorphology.moran(RandomGridGenerator(size).generateGrid)
    }.toArray

    /*
    import org.dianahep.histogrammar._
    val hist = Bin(50,morans.min,morans.max,{d: Double=>d})
    for (d <- morans) hist.fill(d)

    import org.dianahep.histogrammar.ascii._

    println(hist.ascii)
*/

  }

}
