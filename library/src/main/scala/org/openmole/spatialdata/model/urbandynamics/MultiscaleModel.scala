package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Matrix
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation

import scala.util.Random


/**
  * A multiscale model of urban growth, with a strong coupling between scales
  *   Raimbault, J. (2019). A multi-scalar model for system of cities. Conference on Complex Systems, September 2019, Singapore.
  *
  *
  * @param timeSteps number of macro time steps
  * @param macroNcities number of cities (macro parameters)
  * @param macroInitialHierarchy initial hierarchy at the macro level
  * @param macroInitialMaxPop initial max population
  * @param macroRange geographical span of the system
  * @param macroGrowthRate macroscopic growth rate
  * @param macroInteractionDecay macroscopic gravity decay
  * @param macroInteractionWeight macroscopic weight of interaction component
  * @param macroInteractionGamma macroscopic hierarchy
  * @param mesoGridSize size of raster representation of cities (meso parameters)
  * @param mesoCenterDensity density of the center cell
  * @param mesoAlpha aggregation at the mesoscopic scale
  * @param mesoBeta diffusion at the mesoscopic scale
  * @param mesoNdiff number of diffusion steps at the mesoscopic scale
  * @param mesoTimeSteps number of time steps within a macroscopic time step
  * @param macroMesoBetaUpdateMax impact of macro level on beta - in share of beta (coupling parameters)
  * @param macroMesoAlphaUpdateMax impact of macro level on alpha
  * @param mesoMacroCongestionCost impact of meso level on macro level through congestion cost
  * @param mesoMacroDecayUpdateMax impact of meso level on macro decay
  */
case class MultiscaleModel(
                            timeSteps: Int,
                            macroNcities: Int,
                            macroInitialHierarchy: Double,
                            macroInitialMaxPop: Double,
                            macroRange : Double,
                            macroGrowthRate: Double,
                            macroInteractionDecay: Double,
                            macroInteractionWeight: Double,
                            macroInteractionGamma: Double,
                            mesoGridSize: Int,
                            mesoCenterDensity: Double,
                            mesoAlpha: Double,
                            mesoBeta: Double,
                            mesoNdiff: Int,
                            mesoTimeSteps: Int,
                            macroMesoBetaUpdateMax: Double,
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
    val macrostate: MacroState = MacroModel.initialSyntheticState(macroNcities,macroInitialHierarchy,macroInitialMaxPop,macroRange,macroInteractionDecay)
    val mesoStates: Vector[ReactionDiffusionMesoState] = macrostate.populations.flatValues.toVector.map{
      pi => {
        val initState = ReactionDiffusionMesoState.initialSyntheticState(mesoGridSize, mesoCenterDensity, math.sqrt(pi / (2 * math.Pi * mesoCenterDensity)), mesoAlpha,
          mesoBeta, mesoNdiff, 0.0, mesoTimeSteps)
        //assert(pi==initState.populationGrid.flatten.sum,"inc init state : pi = "+pi+" ; meso = "+initState.populationGrid.flatten.sum+"\n"+
        // initState.populationGrid.map(_.map(math.floor)).mkString("\n"))
        initState
      }
    }

    val consistentMacroState: MacroState = MultiscaleModel.ensureConsistence(macrostate,mesoStates,"initialisation")

    assert(mesoStates.forall{_.populationGrid.flatten.sum>0},"existing null meso pop grids")

    val initialCongestedFlows = mesoStates.map{s =>
      //assert(s.populationGrid.flatten.filter(_.isNaN).size==0,"NaN in pop grid : "+s)
      GridMorphology.congestedFlows(s.populationGrid.map{_.toArray}.toArray,mesoMacroCongestionCost)
    }

    assert(!initialCongestedFlows.exists(_.isNaN),"NaNs congested flows : "+initialCongestedFlows)

    MultiscaleState(
      0,consistentMacroState,
        //.copy(congestedFlows = initialCongestedFlows),
      mesoStates,
      MultiscaleState.Parameters(
        growthRates = Vector.fill(macroNcities)(macroGrowthRate),
        interactionDecays = Vector.fill(macroNcities)(macroInteractionDecay),
        interactionWeights = Vector.fill(macroNcities)(macroInteractionWeight),
        interactionGammas = Vector.fill(macroNcities)(macroInteractionGamma),
        congestedFlows= initialCongestedFlows)
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
    val provMacroState = MacroModel.macroStep(
      state.macroState,
      Vector(
        Gibrat(state.parameters.growthRates),
        InteractionMacro(state.parameters.interactionDecays,state.parameters.interactionWeights,state.parameters.interactionGammas,state.macroState.distanceMatrix)
      )
    )
    val deltas = MacroModel.deltaMacroStates(state.macroState,provMacroState)

    // update meso parameters as a function of delta populations
    val updatedMesoStates = ReactionDiffusionMesoState.mesoStep(state.mesoStates.zip(deltas).map{case (mesoState,delta) =>
      MultiscaleModel.updateMesoParameters(mesoState,delta._1,delta._2,delta._3,macroMesoBetaUpdateMax,macroMesoAlphaUpdateMax)
    })

    // constraint of roughly same pop in meso and macro representations
    val consistentMacroState = MultiscaleModel.ensureConsistence(provMacroState,updatedMesoStates,"model step")

    // update macroparameters as a function of mesoconfigurations
    MultiscaleModel.updateMacroParameters(state.copy(time = state.time+1,macroState = consistentMacroState,mesoStates = updatedMesoStates),mesoMacroCongestionCost,mesoMacroDecayUpdateMax)
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
    * @param mesoState mesoscopic state
    * @param deltaPop population increment
    * @param relDeltaPop relative pop increment
    * @param relDeltaAccess relative accessibility increment
    * @param betaUpdateMax max value for beta upbate
    * @param alphaUpdateMax max value for alpha update
    * @return
    */
  def updateMesoParameters(mesoState: ReactionDiffusionMesoState,deltaPop: Double,relDeltaPop: Double,relDeltaAccess: Double,
                           betaUpdateMax: Double,alphaUpdateMax: Double
                          ): ReactionDiffusionMesoState = {
    val newGrowthRate = deltaPop / mesoState.mesoTimeSteps
    val newBeta = mesoState.beta* (1 + betaUpdateMax*relDeltaPop)
    val newAlpha = mesoState.alpha* (1+ alphaUpdateMax*relDeltaAccess)
    mesoState.copy(beta=newBeta,alpha=newAlpha,growthRate = newGrowthRate)
  }


  /**
    * updates macroscopic parameters :
    *  - only interactionDecay is updated, in a linear feedback by dg = dg (1 + decayMaxUpdate * delta phi / max delta phi)
    *    where the "performance" phi aggregates internal flows with a congestion cost as \sum c_ij = \sum pipj/dij - congestionCost *(pipj/dij)2
    * @param state state
    * @param congestionCost congestion cost
    * @param decayUpdateMax max update of decay
    * @return
    */
  def updateMacroParameters(state: MultiscaleState,
                            congestionCost: Double,
                            decayUpdateMax: Double): MultiscaleState = {

    assert(state.mesoStates.forall{_.populationGrid.flatten.sum>0},"existing null meso pop grids")

    // note: congestedFlow function in spatialdata computes \sum (flow - lambda flow^2 )
    val utilities = state.mesoStates.map{s => GridMorphology.congestedFlows(s.populationGrid.map{_.toArray}.toArray,congestionCost)}

    assert(!utilities.exists(_.isNaN),"Nan in utilities : "+utilities)

    val maxabsu = utilities.map(math.abs).max
    val relutilities = utilities.map{_/maxabsu} // can be negative : too strong neg externalities will decrease the global insertion of the city

    //assert(relutilities.filter(_.isNaN).size==0,"Nan in rel utils : "+maxabsu+" ; "+relutilities)

    // update interaction decays and generalized distance matrix
    val newDecays = state.parameters.interactionDecays.zip(relutilities).map{case (d,u) => d*(1 + decayUpdateMax*u)}
    val newGenDistMatrix = MacroModel.updateDistanceMatrix(state.macroState.distanceMatrix,state.parameters.interactionDecays,newDecays)
    state.copy(
      macroState = state.macroState.copy(distanceMatrix = newGenDistMatrix),
      parameters = state.parameters.copy(interactionDecays = newDecays,congestedFlows = utilities)
    )
  }


  /**
    * assert consistence between macroscopic and mesoscopic state
    * @param macroState macro state
    * @param mesoStates meso states
    * @param call call
    * @param threshold threshold
    */
  def ensureConsistence(macroState: MacroState,
                        mesoStates: Vector[MesoState],
                        call: String = "",
                        threshold: Double = 10000.0
                       )(implicit mImpl: MatrixImplementation): MacroState = {
    val deltaPopLevels = macroState.populations.flatValues.zip(mesoStates).map{case (p,ms)=>math.abs(p - ms.populationGrid.flatten.sum)}
    /*assert(deltaPopLevels.sum/deltaPopLevels.size<threshold,call+" - incoherence between levels : "+deltaPopLevels+"\n"+macroState.populations+"\n"+
      mesoStates.map(_.populationGrid.flatten.sum)
    )*/
    utils.log("total consistence adj = "+deltaPopLevels.map(math.abs).sum)
    macroState.copy(
      populations = Matrix(mesoStates.map(_.populationGrid.flatten.sum).toArray,row=false)
    )
  }




}
