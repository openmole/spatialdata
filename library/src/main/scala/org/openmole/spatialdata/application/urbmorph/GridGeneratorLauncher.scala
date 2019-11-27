
package org.openmole.spatialdata.application.urbmorph

import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.measures._
import org.openmole.spatialdata.grid.synthetic._

import scala.util.Random

/**
  *
  * Explorator for morphology of binary grids
  *
  * @param generatorType
  * @param gridSize
  *
  * @param expMixtureCenters
  * @param expMixture
  */
case class GridGeneratorLauncher(
                                generatorType: String,

                                /**
                                  * Size of the (square) grid
                                  */
                                gridSize: Int,

                                /**
                                  * Random
                                  */
                                randomDensity: Double,

                                /**
                                  * ExpMixture
                                  */
                                expMixtureCenters: Int,
                                expMixtureRadius: Double,
                                expMixtureThreshold: Double,

                                /**
                                  * blocks
                                  */
                                blocksNumber: Int,
                                blocksMinSize: Int,
                                blocksMaxSize: Int,

                                /**
                                  * percolation
                                  */
                                percolationProba: Double,
                                percolationBordPoints: Int,
                                percolationLinkWidth: Double


                                ) {

  /**
    *
    * @param rng
    * @return
    */
  def getGrid(implicit rng: Random) = {
    val world: Array[Array[Double]] = generatorType match {
      case "random" => RandomGridGenerator(gridSize).generateGrid(rng).map{_.map{case d => if(d < randomDensity) 1.0 else 0.0}}
      case "expMixture" => {
        val intgrid = ExpMixtureGenerator(gridSize,expMixtureCenters,1.0,expMixtureRadius).generateGrid(rng)
        val maxval = intgrid.flatten.max
        intgrid.map{_.map{case d => if(d / maxval > expMixtureThreshold) 1.0 else 0.0}}
      }
      case "blocks" => BlocksGridGenerator(gridSize,blocksNumber,blocksMinSize,blocksMaxSize).generateGrid(rng).map{_.map{case d => if(d> 0.0) 1.0 else 0.0}}
      case "percolation" => PercolationGridGenerator(gridSize,percolationProba,percolationBordPoints,percolationLinkWidth,10000).generateGrid(rng)
      case _             ⇒ { assert(false, "Error : the requested generator does not exist"); Array.empty }
    }
    if (GridGeneratorLauncher.density(world) > 0.8) GridGeneratorLauncher.emptyGrid(world) else world
  }

  /**
    *
    * @param rng
    * @return
    */
  def getMorphology(implicit rng: Random): GridMorphology = {
    val grid = getGrid
    GridMorphology(grid)
  }

}


object GridGeneratorLauncher {

  def density(world: Array[Array[Double]]): Double = world.flatten.map { x ⇒ if (x > 0.0) 1.0 else 0.0 }.sum / world.flatten.size

  def emptyGrid(array: Array[Array[Double]]): Array[Array[Double]] = Array.tabulate(array.length)(i ⇒ Array.fill(array(i).length)(0.0))


}


