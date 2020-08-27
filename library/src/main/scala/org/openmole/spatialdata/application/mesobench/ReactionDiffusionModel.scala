package org.openmole.spatialdata.application.mesobench

import org.openmole.spatialdata.grid.synthetic.ReactionDiffusionGridGenerator
import org.openmole.spatialdata.grid.Implicits._
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.measures.GridMorphology._

import scala.util.Random

case class ReactionDiffusionModel(
                                 width: Double,
                                 height: Double,
                                 alpha: Double,
                                 beta: Double,
                                 diffusionSteps: Double,
                                 totalPopulation: Double,
                                 growthRate: Double,
                                 seed: Long
                                 ) {


  def runModel: (Double,Double,Double,Double) = {
    implicit val rng: Random = new Random(seed)
    val generated = ReactionDiffusionGridGenerator(
      size = (width.toInt,height.toInt),
      growthRate = growthRate.toInt,
      totalPopulation = totalPopulation.toInt,
      alpha = alpha,
      beta = beta,
      diffusionSteps = diffusionSteps.toInt
    ).generateGrid
    val morphology = GridMorphology(generated,Seq(Moran(),AverageDistance(),Entropy(),Slope()))
    (morphology.moran,morphology.avgDistance,morphology.entropy,morphology.slope._1)
  }

}
