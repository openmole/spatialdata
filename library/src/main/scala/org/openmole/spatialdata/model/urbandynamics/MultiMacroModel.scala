package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.model.urbandynamics.MultiMacroModel.MultiMacroState
import org.openmole.spatialdata.utils.math.{EmptyMatrix, Matrix}

import scala.collection.mutable.ArrayBuffer

case class MultiMacroModel(
                          models: Seq[MacroModel]
                          ) extends MacroModel {

  override def run: MacroResult = MultiMacroModel.run(this)

  override def nextStep(state: MacroState, populations: Matrix, distanceMatrix: Matrix): MacroState = MultiMacroModel.nextStep(this, state.asInstanceOf[MultiMacroState])


}



object MultiMacroModel {

  case class MultiMacroState(
                            time: Int,
                            populations: Matrix,
                            distanceMatrix: Matrix,
                            modelStates: Seq[MacroState]
                            ) extends MacroState

  case class MultiMacroResult(
                             states: Seq[Seq[MacroState]]
                             ) {

  }

  /**
   * Heuristic to couple model components?
   *  - sequential: update population and distance matrix of each state at each subtimestep
   *  - parallel: run all model from previous synthetic state in parallel; pb: how to combine the results? for pop, addition; for dmat? in this case a single
   *     model updating, but in more general cases?
   *   -> remain sequential for now
   *
   * @param model model
   * @param state state
   * @return
   */
  def nextStep(model: MultiMacroModel, state: MultiMacroState): MacroState = {
    val previousStates = state.modelStates
    val nextStates = new ArrayBuffer[MacroState]
    var currentPop = previousStates.head.populations
    var currentDist = previousStates.head.distanceMatrix
    for ((m, s) <- model.models zip previousStates) {
      val updatedState = m.nextStep(s, currentPop, currentDist)
      nextStates.addOne(updatedState)
      currentPop = updatedState.populations
      currentDist = updatedState.distanceMatrix
    }
    MultiMacroState(
      time = state.time + 1,
      populations = currentPop,
      distanceMatrix = currentDist,
      modelStates = nextStates.toSeq
    )
  }

  def run(model: MultiMacroModel): MacroResult = {
    MacroResult(EmptyMatrix(),EmptyMatrix())
  }

}

