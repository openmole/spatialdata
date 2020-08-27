package org.openmole.spatialdata.application.mesobench

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.measures.GridMorphology.{AverageDistance, Entropy, Moran, Slope}
import org.openmole.spatialdata.grid.synthetic.GravityGridGenerator

import scala.util.Random

case class GravityModel(
                       height: Double,
                       width: Double,
                       growthRate: Double,
                       gravity: Double,
                       populationHierarchy: Double,
                       nCenters: Double,
                       totalPopulation: Double,
                       seed: Long
                       ) {

  def runModel: (Double,Double,Double,Double) = {
    implicit val rng: Random = new Random(seed)
    val generated = GravityGridGenerator(
      height = height.toInt,
      width = width.toInt,
      growthRate = growthRate,
      gravity = gravity,
      populationHierarchy = populationHierarchy,
      nCenters = nCenters.toInt,
      totalPopulation = totalPopulation
    ).generateGrid
    val morphology = GridMorphology(generated,Seq(Moran(),AverageDistance(),Entropy(),Slope()))
    (morphology.moran,morphology.avgDistance,morphology.entropy,morphology.slope._1)
  }

}
