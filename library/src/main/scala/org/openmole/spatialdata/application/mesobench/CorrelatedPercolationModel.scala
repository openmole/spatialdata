package org.openmole.spatialdata.application.mesobench

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.measures.GridMorphology.{AverageDistance, Entropy, Moran, Slope}
import org.openmole.spatialdata.grid.synthetic.CorrelatedPercolationGridGenerator

import scala.util.Random

case class CorrelatedPercolationModel(
                                     size: Double,
                                     correlationRange: Double,
                                     nCenters: Double,
                                     maxKernelRadius: Double,
                                     centersPopulationScaling: Double,
                                     seed: Long
                                     ) {


  def runModel: (Double,Double,Double,Double) = {
    implicit val rng: Random = new Random(seed)
    val generated = CorrelatedPercolationGridGenerator(
      gridSize = size.toInt,
      correlationRange = correlationRange,
      binary = false,
      nCenters = nCenters.toInt,
      centersPopulationScaling = centersPopulationScaling,
      maxKernelRadius = maxKernelRadius
    ).generateGrid
    val morphology = GridMorphology(generated,Seq(Moran(),AverageDistance(),Entropy(),Slope()))
    (morphology.moran,morphology.avgDistance,morphology.entropy,morphology.slope._1)
  }
}
