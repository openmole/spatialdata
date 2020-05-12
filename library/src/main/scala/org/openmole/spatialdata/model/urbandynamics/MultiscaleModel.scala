package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Matrix
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation

import scala.util.Random

case class MultiscaleModel(
                            timeSteps: Int,
                            // macro parameters
                            macroNcities: Int,
                            macroInitialHierarchy: Double,
                            macroInitialMaxPop: Double,
                            macroRange : Double,

                            macroGrowthRate: Double,
                            macroInteractionDecay: Double,
                            macroInteractionWeight: Double,
                            macroInteractionGamma: Double,

                            // meso parameters
                            mesoGridSize: Int,
                            mesoCenterDensity: Double,

                            mesoAlpha: Double,
                            mesoBeta: Double,
                            mesoNdiff: Int,
                            mesoTimeSteps: Int,

                            // coupling parameters
                            macroMesoBetaUpdateMax: Double, // in share of beta
                            macroMesoAlphaUpdateMax: Double,
                            mesoMacroCongestionCost: Double,
                            mesoMacroDecayUpdateMax: Double
                          ) {

  /**
    * initial state
    * @param rng rng
    * @return
    */
  def initialState(implicit rng: Random, mImpl: MatrixImplementation): MultiscaleState = {
    val macrostate = InteractionMacroState.initialSyntheticState(macroNcities,macroInitialHierarchy,macroInitialMaxPop,macroRange,macroGrowthRate,macroInteractionDecay,macroInteractionWeight,macroInteractionGamma)
    val mesoStates = macrostate.populations.flatValues.toVector.map{pi => {
      val initState = ReactionDiffusionMesoState.initialSyntheticState(mesoGridSize, mesoCenterDensity, math.sqrt(pi / (2 * math.Pi * mesoCenterDensity)), mesoAlpha,
        mesoBeta, mesoNdiff, 0.0, mesoTimeSteps)
      //assert(pi==initState.populationGrid.flatten.sum,"inc init state : pi = "+pi+" ; meso = "+initState.populationGrid.flatten.sum+"\n"+
      // initState.populationGrid.map(_.map(math.floor)).mkString("\n"))
      initState
    }
    }

    val consistentMacroState = MultiscaleModel.ensureConsistence(macrostate,mesoStates,"initialisation")

    assert(mesoStates.forall{_.populationGrid.flatten.sum>0},"existing null meso pop grids")

    val initialCongestedFlows = mesoStates.map{s =>
      //assert(s.populationGrid.flatten.filter(_.isNaN).size==0,"NaN in pop grid : "+s)
      GridMorphology.congestedFlows(s.populationGrid.map{_.toArray}.toArray,mesoMacroCongestionCost)
    }

    assert(!initialCongestedFlows.exists(_.isNaN),"NaNs congested flows : "+initialCongestedFlows)

    MultiscaleState(
      0,consistentMacroState.copy(congestedFlows = initialCongestedFlows),
      mesoStates
    )
  }


  /**
    * one step of the model
    * @param state state
    * @param rng rng
    * @return
    */
  def modelStep(state: MultiscaleState)(implicit rng: Random, matrixImplementation: MatrixImplementation): MultiscaleState = {
    // compute next macro state
    val provMacroState = InteractionMacro.macroStep(
      state.macroState,
      Vector(
        _ => Matrix(state.macroState.growthRates.toArray,row=false),
        p => InteractionMacro.interactionGrowthRates(p,state.macroState.distanceMatrix,state.macroState.interactionWeights,state.macroState.interactionGammas)
      )
    )
    val deltas = InteractionMacro.deltaMacroStates(state.macroState,provMacroState)

    // update meso parameters as a function of delta populations
    val updatedMesoStates = ReactionDiffusionMesoState.mesoStep(state.mesoStates.zip(deltas).map{case (mesoState,delta) =>
      MultiscaleModel.updateMesoParameters(mesoState,delta._1,delta._2,delta._3,macroMesoBetaUpdateMax,macroMesoAlphaUpdateMax)
    })

    // constraint of roughly same pop in meso and macro representations
    val consistentMacroState = MultiscaleModel.ensureConsistence(provMacroState,updatedMesoStates,"model step")

    // update macroparameters as a function of mesoconfigurations
    val updatedMacroState = MultiscaleModel.updateMacroParameters(consistentMacroState,updatedMesoStates,mesoMacroCongestionCost,mesoMacroDecayUpdateMax)

    //construct new multiscale state
    MultiscaleState(state.time+1,updatedMacroState,updatedMesoStates)
  }

  /**
    * run the model
    * @param rng rng
    * @return
    */
  def modelRun(fullTimeSeries: Boolean)(implicit rng: Random, mImpl: MatrixImplementation): MultiscaleResult = {
    def run0(steps: Int,state: MultiscaleState, accumulator: Vector[MultiscaleState]): Vector[MultiscaleState] = steps match{
      case 0 => accumulator
      case s =>
        //log("step : "+s)
        val nextState = modelStep(state)
        run0(s - 1,nextState,accumulator++Vector(nextState))
    }
    val init = initialState
    MultiscaleResult(run0(timeSteps,init,Vector(init)),fullTimeSeries)
  }

}



object MultiscaleModel {


  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering


  /**
    * updates mesoscopic parameters :
    *  - growthrate (delta p given)
    *  - beta : simple linear feedback : beta = beta ( 1 + betaUpdateMax . delta P / max delta P)  ( => scenario sprawl or tod)
    *  - alpha : idem => scenario globalized metropolis or eq city
    *  - mesoSteps : could be updated (speed of urban project e.g.) - fixed for now
    *
    * @param mesostate mesoscopic state
    * @param deltaPop population increment
    * @param relDeltaPop relative pop increment
    * @param relDeltaAccess relative accessibility increment
    * @param betaUpdateMax max value for beta upbate
    * @param alphaUpdateMax max value for alpha update
    * @return
    */
  def updateMesoParameters(mesostate: ReactionDiffusionMesoState,deltaPop: Double,relDeltaPop: Double,relDeltaAccess: Double,
                           betaUpdateMax: Double,alphaUpdateMax: Double
                          ): ReactionDiffusionMesoState = {
    val newGrowthRate = deltaPop / mesostate.mesoTimeSteps
    val newBeta = mesostate.beta* (1 + betaUpdateMax*relDeltaPop)
    val newAlpha = mesostate.alpha* (1+ alphaUpdateMax*relDeltaAccess)
    mesostate.copy(beta=newBeta,alpha=newAlpha,growthRate = newGrowthRate)
  }


  /**
    * updates macroscopic parameters :
    *  - only interactionDecay is updated, in a linear feedback by dg = dg (1 + decayMaxUpdate * delta phi / max delta phi)
    *    where the "performance" phi aggregates internal flows with a congestion cost as \sum c_ij = \sum pipj/dij - congestionCost *(pipj/dij)2
    * @param macroState macro states
    * @param mesoStates meso states
    * @param congestionCost congestion cost
    * @param decayUpdateMax max update of decay
    * @return
    */
  def updateMacroParameters(macroState: InteractionMacroState,mesoStates: Vector[ReactionDiffusionMesoState],
                            congestionCost: Double,decayUpdateMax: Double): InteractionMacroState = {

    assert(mesoStates.forall{_.populationGrid.flatten.sum>0},"existing null meso pop grids")

    // note: congestedFlow function in spatialdata computes \sum (flow - lambda flow^2 )
    val utilities = mesoStates.map{s => GridMorphology.congestedFlows(s.populationGrid.map{_.toArray}.toArray,congestionCost)}

    assert(!utilities.exists(_.isNaN),"Nan in utilities : "+utilities)

    val maxabsu = utilities.map(math.abs).max
    val relutilities = utilities.map{_/maxabsu} // can be negative : too strong neg externalities will decrease the global insertion of the city

    //assert(relutilities.filter(_.isNaN).size==0,"Nan in rel utils : "+maxabsu+" ; "+relutilities)

    // update interaction decays and generalized distance matrix
    val newDecays = macroState.interactionDecays.zip(relutilities).map{case (d,u) => d*(1 + decayUpdateMax*u)}
    val newGenDistMatrix = InteractionMacro.updateDistanceMatrix(macroState.distanceMatrix,macroState.interactionDecays,newDecays)
    macroState.copy(interactionDecays = newDecays,distanceMatrix = newGenDistMatrix,congestedFlows = utilities)
  }


  /**
    * assert consistence between macroscopic and mesoscopic state
    * @param macroState macro state
    * @param mesoStates meso states
    * @param call call
    * @param threshold threshold
    */
  def ensureConsistence(macroState: InteractionMacroState,
                        mesoStates: Vector[ReactionDiffusionMesoState],
                        call: String = "",
                        threshold: Double = 10000.0
                       )(implicit mImpl: MatrixImplementation): InteractionMacroState = {
    val deltaPopLevels = macroState.populations.flatValues.zip(mesoStates).map{case (p,ms)=>math.abs(p - ms.populationGrid.flatten.sum)}
    /*assert(deltaPopLevels.sum/deltaPopLevels.size<threshold,call+" - incoherence between levels : "+deltaPopLevels+"\n"+macroState.populations+"\n"+
      mesoStates.map(_.populationGrid.flatten.sum)
    )*/
    utils.log("total consistence adj = "+deltaPopLevels.map(math.abs).sum)
    macroState.copy(populations = Matrix(mesoStates.map(_.populationGrid.flatten.sum).toArray,row=false))
  }




}
