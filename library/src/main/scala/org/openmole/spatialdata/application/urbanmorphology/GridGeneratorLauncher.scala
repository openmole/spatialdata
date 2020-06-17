
package org.openmole.spatialdata.application.urbanmorphology

import org.openmole.spatialdata.grid.Implicits._
import org.openmole.spatialdata.grid.measures._
import org.openmole.spatialdata.grid.synthetic._

import scala.util.Random


/**
  * Explorator for morphology of binary grids
  *
  * @param generatorType type
  * @param gridSize Size of the (square) grid
  * @param randomDensity Random
  * @param expMixtureCenters ExpMixture centers
  * @param expMixtureRadius radius
  * @param expMixtureThreshold threshold
  * @param blocksNumber Blocks number
  * @param blocksMinSize min size
  * @param blocksMaxSize max size
  * @param percolationProba percolation proba
  * @param percolationBordPoints bord points
  * @param percolationLinkWidth link width
  */
case class GridGeneratorLauncher(
                                generatorType: String,
                                gridSize: Int,
                                randomDensity: Double,
                                expMixtureCenters: Int,
                                expMixtureRadius: Double,
                                expMixtureThreshold: Double,
                                blocksNumber: Int,
                                blocksMinSize: Int,
                                blocksMaxSize: Int,
                                percolationProba: Double,
                                percolationBordPoints: Int,
                                percolationLinkWidth: Double
                                ) {

  /**
    *
    * @param rng rng
    * @return
    */
  def getGrid(implicit rng: Random): Array[Array[Double]] = {
    val world: Array[Array[Double]] = generatorType match {
      case "random" => RandomGridGenerator(gridSize).generateGrid(rng).map{_.map{ d => (if(d < randomDensity) 1.0 else 0.0).asInstanceOf[Double]}}
      case "expMixture" =>
        val intgrid = ExpMixtureGridGenerator(gridSize,expMixtureCenters,1.0,Seq.fill(expMixtureCenters)(expMixtureRadius)).generateGrid(rng)
        val maxval = intgrid.flatten.max(Ordering.Double.TotalOrdering)
        intgrid.map{_.map{d => (if(d / maxval > expMixtureThreshold) 1.0 else 0.0).asInstanceOf[Double]}}
      case "blocks" => BlocksGridGenerator(gridSize,blocksNumber,blocksMinSize,blocksMaxSize).generateGrid(rng).map{_.map{ d => (if(d> 0.0) 1.0 else 0.0).asInstanceOf[Double]}}
      case "percolation" => PercolationGridGenerator(gridSize,percolationProba,percolationBordPoints,percolationLinkWidth,10000).generateGrid(rng)
      case _             =>
        assert(assertion = false, "Error : the requested generator does not exist")
        Array.empty
    }
    if (GridGeneratorLauncher.density(world) > 0.8) GridGeneratorLauncher.emptyGrid(world) else world
  }

  /**
    *
    * @param rng rng
    * @return
    */
  def getMorphology(implicit rng: Random): GridMorphology = {
    val grid = getGrid
    GridMorphology(grid)
  }

}


object GridGeneratorLauncher {

  def density(world: Array[Array[Double]]): Double = world.flatten.map { x => if (x > 0.0) 1.0 else 0.0 }.sum / world.flatten.size

  def emptyGrid(array: Array[Array[Double]]): Array[Array[Double]] = Array.tabulate(array.length)(i => Array.fill(array(i).length)(0.0))


}


