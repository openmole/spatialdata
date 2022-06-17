package org.openmole.spatialdata.model.urbandynamics


case class MultiscaleState(
                            time: Int,
                            macroState: MacroStateGen,
                            mesoStates: Vector[ReactionDiffusionMesoState],
                            parameters: MultiscaleState.Parameters
                          )

object MultiscaleState {

  case class Parameters(
                         growthRates: Vector[Double],
                         interactionDecays: Vector[Double],
                         interactionWeights: Vector[Double],
                         interactionGammas: Vector[Double],
                         congestedFlows: Vector[Double]
                       )

}