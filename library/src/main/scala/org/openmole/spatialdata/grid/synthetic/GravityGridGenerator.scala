package org.openmole.spatialdata.grid.synthetic

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.{GridGenerator, RasterLayerData}
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random

/**
  * Gravity based iterative urban growth model described in
  * Li, Y., Rybski, D., & Kropp, J. P. (2019). Singularity cities. Environment and Planning B: Urban Analytics and City Science, 2399808319843534.
  *
  * The model is extended by
  *  - making it more easily to have more centers by seeding a certain number of initial settlements instead of a single individual in the middle (! -> do the same for reaction diffusion! - but stationary state will have a single one still)
  *  - adding a population hierarchy parameter (gives the initial model when = 1.0)
  *
  * Note: should cells self-attract also? - then give the reaction-diffusion model with beta = 0 and gamma=\infty
  *  (the two models are limit cases of each other! - beside how the iteration is done (all cells drawn - proba for each is different than probas among all))
  *
  * @param gridSize grid size
  * @param growthRate growth rate (rescale probabilities)
  * @param gravity gravity exponent
  * @param populationHierarchy population hierarchy
  * @param nCenters number of centers
  */
case class GravityGridGenerator(
                                gridSize: Int,
                                growthRate: Double,
                                gravity: Double,
                                populationHierarchy: Double,
                                nCenters: Int,
                                totalPopulation: Double
                               ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = GravityGridGenerator.gravityGrid(gridSize, growthRate, gravity, populationHierarchy, nCenters, totalPopulation)

}



object GravityGridGenerator {

  def gravityGrid(gridSize: Int,
                  growthRate: Double,
                  gravity: Double,
                  populationHierarchy: Double,
                  nCenters: Int,
                  totalPopulation: Double
                 )(implicit rng: Random): Array[Array[Double]] = {
    val initialcoords: Seq[(Int,Int)] = Stochastic.sampleWithReplacement(for {
      i <- 0 until gridSize
      j <- 0 until gridSize
    } yield (i,j), nCenters)
    val res = Array.fill(gridSize,gridSize)(0.0)
    initialcoords.foreach{case (i0,j0) => res(i0)(j0) = res(i0)(j0) + 1.0}

    val gDistances = GridMorphology.distanceMatrix(2 * gridSize - 1,2 * gridSize - 1).map(_.map(d=> if (d==0.0) 0.0 else math.pow(_, - gravity)))

    Array.empty
  }

}
