package org.openmole.spatialdata.application.sdg

import org.openmole.spatialdata.model.urbandynamics.Coevolution.CoevolutionState
import org.openmole.spatialdata.model.urbandynamics.EconomicExchanges.EconomicExchangesState
import org.openmole.spatialdata.model.urbandynamics.Innovation.{InnovationState, InnovationUtilityLogNormalDistribution, InnovationUtilityNormalDistribution, mutationInnovation}
import org.openmole.spatialdata.model.urbandynamics.MultiMacroModel.MultiMacroResult
import org.openmole.spatialdata.model.urbandynamics.{Coevolution, EconomicExchanges, Innovation, MultiMacroModel}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.utils.math.{DenseMatrix, EmptyMatrix, Matrix, RealMatrix, Statistics}
import org.openmole.spatialdata.vector.measures.Spatstat
import org.openmole.spatialdata.vector.synthetic.RandomPointsGenerator

import scala.util.Random

case class SDG()

object SDG {

  case class Result(result: MultiMacroResult)

  /**
    *  ! matrix implementations are not well tackled - often default impl hardcoded within models -> possible compatibility issues?
    *
    *  - order of submodels? arbitrary: innov, eco, coevol
    *  - initial state: better as a field in MultiMacroModel
    *  - no gibrat (just adds some kind of offset, does not change trade-offs? maybe hierarchy regime? no as hierarchy is kept)
    *  - could add param classes for each model? ~
    *  - MultiMacroRun should be provided initial state -> construct here using proper function for each submodel
    *  - aggreg emission as output indic? -> compute same unit for eco
    *  - ! too many param -> common gravity interactions?
    *  - Q of complex interactions between submodules?: specific question for model validation
    * @return
    */
  def runSyntheticMultiMacroModel(
                                syntheticCities: Int,
                                syntheticHierarchy: Double,
                                syntheticMaxPop: Double,
                                finalTime: Int,
                                seed: Long,
                                innovationWeight: Double,
                                innovationGravityDecay: Double,
                                innovationInnovationDecay: Double,
                                innovationMutationRate: Double,
                                innovationNewInnovationHierarchy: Double,
                                innovationEarlyAdoptersRate: Double,
                                innovationUtilityStd: Double,
                                innovationUtilityDistrib: String,
                                ecoWeight: Double,
                                ecoSizeEffectOnDemand: Double,
                                ecoSizeEffectOnSupply: Double,
                                ecoGravityDecay: Double,
                                ecoWealthToPopulationExponent: Double,
                                ecoPopulationToWealthExponent: Double,
                                coevolWeight: Double,
                                coevolGamma: Double,
                                coevolGravityDecay: Double,
                                coevolNetworkGmax: Double,
                                coevolNetworkExponent: Double,
                                coevolNetworkThresholdQuantile: Double
                              )(implicit rng: Random): Result = {
    implicit val m: MatrixImplementation = Matrix.defaultImplementation
    rng.setSeed(seed)

    // first generate synthetic cities: population and coordinates
    // similar to synthetic innovation
    val dmat = Matrix(Spatstat.euclidianDistanceMatrix(RandomPointsGenerator(syntheticCities).generatePoints.asPointSeq.toArray))
    val initialPopulations = Statistics.rankSizeDistribution(syntheticCities, syntheticHierarchy, syntheticMaxPop)

    // full pop matrix needed because of the way single models are implemented (for real data) - but dropped later in MultiMacroStates
    val populationMatrix = DenseMatrix.zeros(syntheticCities,finalTime+1)
    populationMatrix.setMSubmat(0,0,Array(initialPopulations.toArray).transpose)
    val dates: Array[Double] = (0 to finalTime).toArray.map{_.toDouble}

    // note: constructor are a mess: legacy vs mutation model, no proper for mutation
    // initial innov utility is 1
    val distrib = innovationUtilityDistrib match {case "normal" => InnovationUtilityNormalDistribution(); case "log-normal" => InnovationUtilityLogNormalDistribution()}
    val innovModel = Innovation(populationMatrix,dmat,dates,rng,0.0,innovationWeight,innovationGravityDecay,innovationInnovationDecay,
      mutationInnovation(_,_,_, innovationMutationRate, innovationNewInnovationHierarchy, innovationEarlyAdoptersRate, innovationUtilityStd, distrib),
      1.0
    )
    val innovInitialState = Innovation.initialState(innovModel)

    // eco exchanges - ! add gravitypotentials in eco state (total emission: cumulated interactions across models)
    // ! issue: not same unit for innov / eco (supply - demand gravity potentials) -> should compute something equivalent?
    // ! marius weight? - rescale pop updates?
    val ecoModel = EconomicExchanges(populationMatrix, dmat, dates, ecoWeight, ecoSizeEffectOnDemand, ecoSizeEffectOnSupply, ecoGravityDecay, ecoWealthToPopulationExponent, ecoPopulationToWealthExponent)
    val ecoInitialState = EconomicExchanges.initialState(ecoModel)

    // coevolution
    // rq: flows also in other unit (pop rescaled) -> must compute pop-based only for comparability
    val coevolModel = Coevolution(populationMatrix, Array(dmat), EmptyMatrix(), dates, 0.0, coevolWeight, coevolGamma, coevolGravityDecay,
      0.0, 1.0, 1.0, coevolNetworkGmax, coevolNetworkExponent, coevolNetworkThresholdQuantile)
    val coevolInitialState = Coevolution.initialState(coevolModel)

    val model = MultiMacroModel(Seq(innovModel, ecoModel, coevolModel), Seq(innovInitialState, ecoInitialState, coevolInitialState))

    Result(model.run.asInstanceOf[MultiMacroResult])
  }


  /**
    * SDG 13: Climate - minimised
    * Emission flows: cumulated across models, time and city pairs
    * @param res res
    * @return
    */
  def cumulatedFlows(res: Result): Double = {
    val t = res.result.states.length
    //utils.log(res.submodelStates[InnovationState].mkString(" ; "))
    (res.result.submodelStates[InnovationState].map(_.asInstanceOf[InnovationState].flows).map(m => m.sum / (m.ncols*m.nrows)).sum +
      res.result.submodelStates[EconomicExchangesState].map(_.asInstanceOf[EconomicExchangesState].flows).map(m => m.sum / (m.ncols*m.nrows)).sum +
      res.result.submodelStates[CoevolutionState].map(_.asInstanceOf[CoevolutionState].flows).map(m => m.sum / (m.ncols*m.nrows)).sum) / t
  }

  /**
    * SDG 8: Innovation - maximised
    * @param res res
    * @return
    */
  def averageUtility(res: Result): Double = {
    val innovationShares = res.result.submodelStates[InnovationState].last.asInstanceOf[InnovationState].innovations
    val innovationUtilities = res.result.submodelStates[InnovationState].last.asInstanceOf[InnovationState].utilities
    val normPop = res.result.simulatedPopulation%*%DenseMatrix.diagonal(res.result.simulatedPopulation.colSum.map(1/_))
    innovationShares.zip(innovationUtilities).map{case (m,u)=> (normPop*m*(u/m.ncols)).sum}.sum
  }

  /**
    * SDG 9: infrastructure: average distance between cities - minimised
    *  (not weighted by pop - should also look at access?) - no, at least weight by O/D -> avg dist between indivs
    * @param res res
    * @return
    */
  def averageDistance(res: Result): Double = res.result.states.map{s =>
    val ptot = s.populations.sum
    val diagpop = DenseMatrix.diagonal(s.populations.flatValues.map( _ / ptot))
    (diagpop %*% s.distanceMatrix %*% diagpop).sum
  }.sum / res.result.states.length

  /**
    * SDG 10: economic inequality - minimised
    *  Gini index given by \sum_{i,j} \abs{W_i - W_j} / 2 n*n*\bar{W}, averaged in time
    *
    *  ! must constraint Delta W > 0, otherwise cases where W converges to zero (negative) yield a 0 Gini -> goal 1: no poverty
    * @param res res
    * @return
    */
  def giniEconomicWealth(res: Result): Double = {
    val ginis = res.result.submodelStates[EconomicExchangesState].map{s =>
      val w = s.asInstanceOf[EconomicExchangesState].wealths
      val avgw = w.sum / w.length
      val n = w.length
      w.flatMap(wi => w.map(wj => math.abs(wi - wj))).sum / (2*n*n*avgw)
    }
    ginis.sum / ginis.length
  }

  /**
    * SDG 1: total wealth - maximised
    *   Rq: better index should be fraction of individual incomes below a threshold
    * @param res res
    * @return
    */
  def averageWealth(res: Result): Double = {
    val allw = res.result.submodelStates[EconomicExchangesState].flatMap(_.asInstanceOf[EconomicExchangesState].wealths)
    allw.sum / allw.length
  }


}
