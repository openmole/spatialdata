package org.openmole.spatialdata.application.multimodelmacro

import org.openmole.spatialdata.model.urbandynamics.MultiMacroModel.MultiMacroResult
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Statistics

import scala.util.Random

/**
  * Weight parameters not really comparable? order of magnitude of delta P seems to be 10 in "default" setting
  */
object RunSDG extends App {

  implicit val rng: Random = new Random

  val mode = "projection" // "synthetic", "real"

  val initialState = mode match {
    case "synthetic" => SDG.setupSynthetic(syntheticCities = 30,
      syntheticHierarchy = 1.0,
      syntheticMaxPop = 100000.0,
      finalTime = 20, // limit to avoid huge values: 50 -> up to larger than 1e9 !
      seed = 42L
    )
    case "real" => RealData.setupRealDataPastTrajectories("data/urbansystems/EU_pops.csv", "data/urbansystems/EU_dist.csv", "data/urbansystems/EU_dates.csv")
    case "projection" => RealData.setupRealDataFutureTrajectories("data/urbansystems/EU_pops.csv", "data/urbansystems/EU_dist.csv",3)
  }

  val result: SDG.Result = SDG.runMultiModelMacro(
    SDG.setupMultiModelMacro(
      initialState._1, initialState._2, initialState._3,
      seed = rng.nextInt(),
      innovationWeight = 1e-2, // 0.005
      innovationGravityDecay = 1000.0,//0.5,
      innovationInnovationDecay = 1000.0,
      innovationMutationRate = 0.2,
      innovationNewInnovationHierarchy = 0.5,
      innovationEarlyAdoptersRate = 0.2,
      innovationUtilityStd = 1.0,
      innovationUtilityDistrib = "log-normal",
      ecoWeight = 1e-2,
      ecoMultiplier = 2.0,
      ecoSizeEffectOnDemand = 0.1,
      ecoSizeEffectOnSupply = 0.2,
      ecoGravityDecay = 1000.0,//0.5,
      ecoWealthToPopulationExponent = 1.0,
      ecoPopulationToWealthExponent = 1.0,
      coevolWeight = 1e-2,
      coevolGamma = 1.0,
      coevolGravityDecay = 1000.0,
      coevolNetworkGmax = 0.0001,
      coevolNetworkExponent = 1.0,
      coevolNetworkThresholdQuantile = 0.5
    )
  )

  utils.log(s"Indicators: Emissions = ${SDG.cumulatedFlows(result)}; Innovation = ${- SDG.averageUtility(result)}; Infrastructure = ${SDG.averageDistance(result)}; Eco inequality = ${SDG.giniEconomicWealth(result)}; Wealth = ${SDG.averageWealth(result)}")

  val realresult: RealData.Result = RealData.Result.apply(result.result)

  if(mode=="real") utils.log(s"Fit on populations (real setup): logmse =  ${realresult.logmse} ; mselog = ${realresult.mselog}")
  if(mode=="projection") {
    val n = result.result.simulatedPopulation.nrows
    utils.log(s"Average logpop traj: ${result.result.simulatedPopulation.values.transpose.map{col => col.map(math.log).sum/n}.toSeq}")
    utils.log(s"Std logpop traj: ${result.result.simulatedPopulation.values.transpose.map{col => Statistics.std(col.map(math.log))}.toSeq}")
  }

}
