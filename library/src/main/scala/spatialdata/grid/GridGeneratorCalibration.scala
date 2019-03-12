
package spatialdata.grid

import spatialdata.measures.Morphology
import spatialdata.synthetic.grid.{BlocksGridGenerator, ExpMixtureGenerator, PercolationGridGenerator}
import spatialdata._

import scala.util.Random


object GridGeneratorCalibration {


  trait Calibration {

    /**
      * mono-objective calibration
      * @param projection
      * @param objective
      * @param rng
      * @return
      */
    def calibrate(projection: Morphology => Array[Double],objective: Array[Double]=> Double)(implicit rng : Random): Double

  }



  case class CalibrateBlocksGridGenerator(
                                           gridSize: Int,
                                           blocksNumber: Int,
                                           blocksMinSize: Int,
                                           blocksMaxSize: Int
                                         ) extends Calibration {

    override def calibrate(projection: Morphology => Array[Double],objective: Array[Double]=> Double)(implicit rng : Random): Double = {
      objective(projection(Morphology(BlocksGridGenerator(gridSize,blocksNumber,blocksMinSize,blocksMaxSize).generateGrid(rng).map{_.map{case d => if(d> 0.0) 1.0 else 0.0}})))
    }
  }

  case class CalibrateExpMixtureGridGenerator(
                                             gridSize: Int,
                                             expCenters: Int,
                                             expRadius: Double,
                                             expThreshold: Double
                                             ) extends Calibration {
    override def calibrate(projection: Morphology => Array[Double], objective: Array[Double] => Double)(implicit rng: Random): Double = {
      val intgrid = ExpMixtureGenerator(gridSize,expCenters,1.0,expRadius).generateGrid(rng)
      val maxval = intgrid.flatten.max
      objective(projection(Morphology(intgrid.map{_.map{case d => if(d / maxval > expThreshold) 1.0 else 0.0}})))
    }
  }

  case class CalibratePercolationGenerator(
                                          gridSize: Int,
                                          percolationProba: Double,
                                          percolationBordPoints: Int,
                                          percolationLinkWidth: Double
                                          ) extends Calibration {
    override def calibrate(projection: Morphology => Array[Double], objective: Array[Double] => Double)(implicit rng: Random): Double = {
      objective(projection(Morphology(PercolationGridGenerator(gridSize,percolationProba,percolationBordPoints,percolationLinkWidth).generateGrid(rng))))
    }
  }

}


