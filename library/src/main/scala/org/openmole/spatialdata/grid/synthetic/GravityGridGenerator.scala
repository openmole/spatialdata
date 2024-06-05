package org.openmole.spatialdata.grid.synthetic

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.{GridGenerator, RasterLayerData}
import org.openmole.spatialdata.utils.math.{Convolution, Stochastic}

import scala.util.Random

/**
  * Gravity based iterative urban growth model described in
  * Li, Y., Rybski, D., & Kropp, J. P. (2019). Singularity cities. Environment and Planning B: Urban Analytics and City Science, 2399808319843534.
  *
  * The model is extended by
  *  - making it more easily to have more centers by seeding a certain number of initial settlements instead of a single individual in the middle (! -> do the same for reaction diffusion! - but stationary state will have a single one still)
  *  - adding a population hierarchy parameter (gives the initial model when = 1.0)
  *  - stop iterations when pop >= total population
  *
  * Note: should cells self-attract also? - then give the reaction-diffusion model with beta = 0 and gamma=\infty
  *  (the two models are limit cases of each other! - beside how the iteration is done (all cells drawn - proba for each is different than probas among all))
  *
  * @param gridSize grid size
  * @param growthRate growth rate (rescale probabilities)
  * @param gravity gravity exponent
  * @param populationHierarchy population hierarchy
  * @param nCenters number of centers
  * @param totalPopulation total population
  */
case class GravityGridGenerator(
                                height: Int,
                                width: Int,
                                growthRate: Double,
                                gravity: Double,
                                populationHierarchy: Double,
                                nCenters: Int,
                                totalPopulation: Double
                               ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = GravityGridGenerator.gravityGrid(height, width, growthRate, gravity, populationHierarchy, nCenters, totalPopulation)

}



object GravityGridGenerator {

  def gravityGrid(height: Int,
                  width: Int,
                  growthRate: Double,
                  gravity: Double,
                  populationHierarchy: Double,
                  nCenters: Int,
                  totalPopulation: Double
                 )(implicit rng: Random): Array[Array[Double]] = {
    val initialcoords: Seq[(Int,Int)] = Stochastic.sampleWithReplacement(for {
      i <- 0 until height
      j <- 0 until width
    } yield (i,j), nCenters)
    val res = Array.fill(height,width)(0.0)
    initialcoords.foreach{case (i0,j0) => res(i0)(j0) = res(i0)(j0) + 1.0}

    val gDistances: Array[Array[Double]] = GridMorphology.distanceMatrix(2 * height - 1,2 * width - 1).map {
      (row: Array[Double]) => row.map{(d: Double) => if (d == 0.0) 0.0 else math.pow(d, -gravity)}
    }
    val probadenom: Array[Double] = Convolution.convolution2D(Array.fill(height,width)(1.0),gDistances).flatten

    def iteration(prevPop: Seq[Seq[Double]]): Seq[Seq[Double]] = {
      val probanum: Array[Double] = Convolution.convolution2D(prevPop.map(_.map(d => math.pow(d,populationHierarchy)).toArray).toArray,gDistances).flatten
      val provprobas = probanum.zip(probadenom).map{case (num,denom) => num/denom}
      val maxproba = provprobas.max(Ordering.Double.TotalOrdering)
      val probas = provprobas.map(_ * growthRate / maxproba)

      val nextPops = Array.fill(height,width)(0.0)

      probas.zip(for {
        i <- 0 until height
        j <- 0 until width
      } yield (i,j)).foreach{
        case (p,(i,j)) => if (rng.nextDouble() < p) nextPops(i)(j) = prevPop(i)(j) + 1 else nextPops(i)(j) = prevPop(i)(j)
      }
      //println(nextPops.flatten.sum)
      nextPops.map(_.toSeq).toSeq
    }

    val finalpop = Iterator.iterate(res.map(_.toSeq).toSeq)(iteration).takeWhile(_.flatten.sum < totalPopulation).toSeq.last

    finalpop.map(_.toArray).toArray
  }

}
