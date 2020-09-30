package org.openmole.spatialdata.application.mesobench


import scala.util.Random

object RunMesoBench extends App {

  implicit val rng: Random = new Random

  val (width,height) = (100.0,200.0)
  val (alpha,beta,diffusionSteps,totalPopulationRD,growthRateRD) = (2.0,0.1,3.0,60000000.0,300000.0)
  val (centers,hierarchy,maxRadiusRate) = (1.0,1.1,0.1)
  val (correlationRange, nCentersCP, maxKernelRadius, centersPopulationScaling) = ( 10.0, 10.0,10.0,1.1)
  val (growthRateG, gravity, populationHierarchy, nCentersG, totalPopulationG) = (0.1,2.0,2.0,5.0,10000.0)

  val start = System.currentTimeMillis()
  //val morpho = ReactionDiffusionModel(width,height,alpha,beta,diffusionSteps,totalPopulation,growthRate,rng.nextLong()).runModel
  val morpho = ExpMixtureModel(width,height,centers,hierarchy,maxRadiusRate,rng.nextLong()).runModel
  //val morpho = CorrelatedPercolationModel(size, correlationRange, nCenters, maxKernelRadius, centersPopulationScaling,rng.nextLong()).runModel
  //val morpho = GravityModel(width, height, growthRate, gravity, populationHierarchy, nCenters, totalPopulation,rng.nextLong()).runModel

  println(s"${System.currentTimeMillis() - start} ms")

  println(morpho)

}
