package org.openmole.spatialdata.application.sdg

import scala.util.Random

object RunSDG extends App {

  implicit val rng: Random = new Random

  val model = SDG.syntheticMultiMacroModel(
    syntheticCities = 30,
    syntheticHierarchy = 1.0,
    syntheticMaxPop =  100000.0,
    finalTime = 10,
    seed = rng.nextInt(),
    innovationWeight = 0.01,
    innovationGravityDecay = 0.5,
    innovationInnovationDecay = 0.3,
    innovationMutationRate = 0.2,
    innovationNewInnovationHierarchy = 0.5,
    innovationEarlyAdoptersRate = 0.2,
    innovationUtilityStd = 1.0,
    innovationUtilityDistrib = "log-normal",
    ecoWeight = 0.01,
    ecoSizeEffectOnDemand = 1.0,
    ecoSizeEffectOnSupply = 1.0,
    ecoGravityDecay = 0.5,
    ecoWealthToPopulationExponent = 1.1,
    ecoPopulationToWealthExponent = 1.2,
    coevolWeight = 0.01,
    coevolGamma = 1.0,
    coevolGravityDecay = 0.5,
    coevolNetworkGmax = 0.05,
    coevolNetworkExponent = 1.0,
    coevolNetworkThresholdQuantile = 0.5
  )

  val result = model.run
}
