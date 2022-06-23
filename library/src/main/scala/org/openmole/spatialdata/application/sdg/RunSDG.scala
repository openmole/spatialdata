package org.openmole.spatialdata.application.sdg

import org.openmole.spatialdata.model.urbandynamics.MultiMacroModel.MultiMacroResult
import org.openmole.spatialdata.utils

import scala.util.Random

/**
  * Weight parameters not really commparable? order of magnitude of delta P seems to be 10 in "default" setting
  */
object RunSDG extends App {

  implicit val rng: Random = new Random

  val result = SDG.runSyntheticMultiMacroModel(
    syntheticCities = 30,
    syntheticHierarchy = 1.0,
    syntheticMaxPop =  100000.0,
    finalTime = 20, // limit to avoid huge values: 50 -> up to larger than 1e9 !
    seed = rng.nextInt(),
    innovationWeight = 0.005,
    innovationGravityDecay = 1.0,//0.5,
    innovationInnovationDecay = 0.3,
    innovationMutationRate = 0.2,
    innovationNewInnovationHierarchy = 0.5,
    innovationEarlyAdoptersRate = 0.2,
    innovationUtilityStd = 1.0,
    innovationUtilityDistrib = "log-normal",
    ecoWeight = 0.05,
    ecoSizeEffectOnDemand = 0.1,
    ecoSizeEffectOnSupply = 0.2,
    ecoGravityDecay = 0.1,//0.5,
    ecoWealthToPopulationExponent = 1.5,
    ecoPopulationToWealthExponent = 1.5,
    coevolWeight = 0.005,
    coevolGamma = 1.0,
    coevolGravityDecay = 0.5,
    coevolNetworkGmax = 0.05,
    coevolNetworkExponent = 1.0,
    coevolNetworkThresholdQuantile = 0.5
  )

  utils.log(s"Indicators: Emissions = ${SDG.cumulatedFlows(result)}; Innovation = ${- SDG.averageUtility(result)}; Infrastructure = ${SDG.averageDistance(result)}; Eco inequality = ${SDG.giniEconomicWealth(result)}; Wealth = ${SDG.averageWealth(result)}")

}
