
package spatialdata.grid

import spatialdata.measures.Morphology
import spatialdata.synthetic.grid.{BlocksGridGenerator, ExpMixtureGenerator, PercolationGridGenerator, RandomGridGenerator}
import spatialdata._

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
  def getMorphology(implicit rng: Random): Morphology = {
    val grid = generatorType match {
      case "random" => RandomGridGenerator(gridSize).generateGrid(rng).map{_.map{case d => if(d < randomDensity) 1.0 else 0.0}}
      case "expMixture" => {
        val intgrid = ExpMixtureGenerator(gridSize,expMixtureCenters,1.0,expMixtureRadius).generateGrid(rng)
        val maxval = intgrid.flatten.max
        intgrid.map{_.map{case d => if(d / maxval > expMixtureThreshold) 1.0 else 0.0}}
      }
      case "blocks" => BlocksGridGenerator(gridSize,blocksNumber,blocksMinSize,blocksMaxSize).generateGrid(rng).map{_.map{case d => if(d> 0.0) 1.0 else 0.0}}
      case "percolation" => PercolationGridGenerator(gridSize,percolationProba,percolationBordPoints,percolationLinkWidth).generateGrid(rng)
    }
    Morphology(grid)
  }

}


object GridGeneratorLauncher


