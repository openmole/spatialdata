package org.openmole.spatialdata.model.urbandynamics

case class MultiscaleState(
                            time: Int,
                            macroState: InteractionMacroState,
                            mesoStates: Vector[ReactionDiffusionMesoState]
                          )