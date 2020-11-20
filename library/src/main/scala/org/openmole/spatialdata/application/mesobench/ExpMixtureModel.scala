package org.openmole.spatialdata.application.mesobench

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.measures.GridMorphology.{AverageDistance, Entropy, Moran, Slope}
import org.openmole.spatialdata.grid.Implicits._
import org.openmole.spatialdata.grid.synthetic.ExpMixtureGridGenerator

import scala.util.Random

/**
  *
  * Taking P_i = P_0 i{-alpha}, we have
  *   r_i = r_0 i{-alpha/2}
  *   Ptot = 2Pi P_max r_0*r_0 \sum i{-alpha}
  *
  * @param width width
  * @param height height
  * @param centers number of centers
  * @param hierarchy rank-size hierarchy between centers
  * @param maxRadiusRate size of larger city radius in proportion of max(height,width)
  * @param seed seed
  */
case class ExpMixtureModel(
                       width: Double,
                       height: Double,
                       centers: Double,
                       hierarchy: Double,
                       maxRadiusRate: Double,
                       seed: Long
                     ) {


  def runModel: (Double,Double,Double,Double) = {
    implicit val rng: Random = new Random(seed)

    //val unscaledPopulations = Statistics.rankSizeDistribution(centers.toInt, hierarchy, 1.0)
    val radius = maxRadiusRate*math.min(width,height)
    val rieman = (1 to centers.toInt).map(i => math.pow(i.toDouble,-hierarchy)).sum
    val density = 1.0 / (2*math.Pi*radius*radius*rieman)
    val radiuses = (1 to centers.toInt).map(i => radius*math.pow(i.toDouble,-hierarchy/2.0))

    val generated = ExpMixtureGridGenerator(
      size = (width.toInt,height.toInt),
      centers = centers.toInt,
      maxValue = density,
      kernelRadiuses = radiuses
    ).generateGrid
    val morphology = GridMorphology(generated,Seq(Moran(),AverageDistance(),Entropy(),Slope()))
    (morphology.moran,morphology.avgDistance,morphology.entropy,morphology.slope._1)
  }

}
