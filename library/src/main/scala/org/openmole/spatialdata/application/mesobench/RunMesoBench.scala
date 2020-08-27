package org.openmole.spatialdata.application.mesobench


import scala.util.Random

object RunMesoBench extends App {

  implicit val rng: Random = new Random

  //val (width,height,alpha,beta,diffusionSteps,totalPopulation,growthRate) = (100.0,200.0,2.0,0.1,3.0,60000000.0,300000.0)
  //val (width,height,centers,hierarchy,maxRadiusRate) = (100.0,200.0,100.0,1.1,0.1)
  //val (size, correlationRange, nCenters, maxKernelRadius, centersPopulationScaling) = (100.0, 10.0, 10.0,10.0,1.1)
  val (width, height,growthRate, gravity, populationHierarchy, nCenters, totalPopulation) = (100.0,200.0,0.1,2.0,2.0,5.0,10000.0)

  val start = System.currentTimeMillis()
  //val morpho = ReactionDiffusionModel(width,height,alpha,beta,diffusionSteps,totalPopulation,growthRate,rng.nextLong()).runModel
  //val morpho = ExpMixtureModel(width,height,centers,hierarchy,maxRadiusRate,rng.nextLong()).runModel
  //val morpho = CorrelatedPercolationModel(size, correlationRange, nCenters, maxKernelRadius, centersPopulationScaling,rng.nextLong()).runModel
  val morpho = GravityModel(width, height, growthRate, gravity, populationHierarchy, nCenters, totalPopulation,rng.nextLong()).runModel

  println(s"${System.currentTimeMillis() - start} ms")

  println(morpho)

}
