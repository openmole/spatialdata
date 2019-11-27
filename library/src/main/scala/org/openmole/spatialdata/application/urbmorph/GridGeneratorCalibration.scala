package org.openmole.spatialdata.application.urbmorph

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.synthetic._
import org.openmole.spatialdata._

import scala.util.Random

object GridGeneratorCalibration {

  /**
    * generic calibration function
    *   note : better do separate scripts - different param names and types
    * @param size
    * @param params
    * @param projection
    * @param objective
    * @param model
    * @param rng
    * @return
    */
  def calibrateModel(size: Int, params: Array[Double], projection: GridMorphology => Array[Double], objective: Array[Double]=> Double, model : String)(implicit rng : Random): Double = {
    model match {
      case "expMixture" =>  CalibrateExpMixtureGridGenerator(size,params(0).toInt,params(1),params(2)).calibrate(projection,objective)
      case "blocks" => CalibrateBlocksGridGenerator(size, params(0).toInt, params(1).toInt, params(2).toInt).calibrate(projection, objective)
      case "percolation" => CalibratePercolationGenerator(size,params(0),params(1).toInt,params(2)).calibrate(projection,objective)
    }
  }


  trait Calibration {

    /**
      * mono-objective calibration
      * @param projection
      * @param objective
      * @param rng
      * @return
      */
    def calibrate(projection: GridMorphology => Array[Double], objective: Array[Double]=> Double)(implicit rng : Random): Double



  }



  case class CalibrateBlocksGridGenerator(
                                           gridSize: Int,
                                           blocksNumber: Int,
                                           blocksMinSize: Int,
                                           blocksMaxSize: Int
                                         ) extends Calibration {

    override def calibrate(projection: GridMorphology => Array[Double], objective: Array[Double]=> Double)(implicit rng : Random): Double = {
      objective(projection(GridMorphology(BlocksGridGenerator(gridSize,blocksNumber,blocksMinSize,blocksMaxSize).generateGrid(rng).map{_.map{case d => if(d> 0.0) 1.0 else 0.0}})))
    }
  }

  case class CalibrateExpMixtureGridGenerator(
                                             gridSize: Int,
                                             expCenters: Int,
                                             expRadius: Double,
                                             expThreshold: Double
                                             ) extends Calibration {
    override def calibrate(projection: GridMorphology => Array[Double], objective: Array[Double] => Double)(implicit rng: Random): Double = {
      val intgrid = ExpMixtureGenerator(gridSize,expCenters,1.0,expRadius).generateGrid(rng)
      val maxval = intgrid.flatten.max
      objective(projection(GridMorphology(intgrid.map{_.map{case d => if(d / maxval > expThreshold) 1.0 else 0.0}})))
    }
  }

  case class CalibratePercolationGenerator(
                                          gridSize: Int,
                                          percolationProba: Double,
                                          percolationBordPoints: Int,
                                          percolationLinkWidth: Double
                                          ) extends Calibration {
    override def calibrate(projection: GridMorphology => Array[Double], objective: Array[Double] => Double)(implicit rng: Random): Double = {
      objective(projection(GridMorphology(PercolationGridGenerator(gridSize,percolationProba,percolationBordPoints,percolationLinkWidth,10000).generateGrid(rng))))
    }
  }

}
