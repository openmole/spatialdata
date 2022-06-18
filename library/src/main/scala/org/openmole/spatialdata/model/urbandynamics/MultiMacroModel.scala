package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.model.urbandynamics.MultiMacroModel.MultiMacroState
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{EmptyMatrix, Matrix}

import scala.collection.mutable.ArrayBuffer

/**
  * A macroscopic urban dynamics model "weakly" coupling several components: population and other dimensions are evolved seuqentially for
  *   each submodel within each time-step
  *    Rq: which typology of model coupling? between totally weak and strong? -> classify/sr-meta anal?
  * @param models coupled models
  */
case class MultiMacroModel(
                          models: Seq[MacroModel],
                          initialStates: Seq[MacroState]
                          ) extends MacroModel {

  override def run: MacroResult = MultiMacroModel.run(this)

  override def nextStep(state: MacroState, populations: Matrix, distanceMatrix: Matrix): MacroState = MultiMacroModel.nextStep(this, state.asInstanceOf[MultiMacroState])

  override def finalTime: Int = models.head.finalTime

}


/**
  * No generic constructor? -> specific in applications (ex: coevol - innov - eco)
  */
object MultiMacroModel {

  case class MultiMacroState(
                            time: Int,
                            populations: Matrix,
                            distanceMatrix: Matrix,
                            modelStates: Seq[MacroState]
                            ) extends MacroState

  case class MultiMacroResult(
                             states: Seq[MultiMacroState]
                             ) extends MacroResult {
    override def simulatedPopulation: Matrix = Matrix(states.map(_.populations.flatValues).toArray.transpose)(Matrix.defaultImplementation)
  }

  /**
   * Heuristic to couple model components?
   *  - sequential: update population and distance matrix of each state at each subtimestep
   *  - parallel: run all model from previous synthetic state in parallel; pb: how to combine the results? for pop, addition; for dmat? in this case a single
   *     model updating, but in more general cases?
   *   -> remain sequential for now
   *
    * ! is consistence across submodels ensured? - they should not share more than pop and distance
    *  -> more commplicated for a stronger coupling
    *  + no synthetic state?: no need? Y - in MultiMacroState
    *
   * @param model model
   * @param state state
   * @return
   */
  def nextStep(model: MultiMacroModel, state: MultiMacroState): MacroState = {

    utils.log(s"\n\n==============\nStep ${state.time}\n==============")

    val previousStates = state.modelStates

    val prevpop = previousStates.head.populations

    val nextStates = new ArrayBuffer[MacroState]
    var currentPop = previousStates.head.populations
    var currentDist = previousStates.head.distanceMatrix
    for ((m, s) <- model.models zip previousStates) {
      val updatedState = m.nextStep(s, currentPop, currentDist)
      nextStates.addOne(updatedState)
      currentPop = updatedState.populations
      currentDist = updatedState.distanceMatrix
    }

    val finalpop = currentPop
    utils.log(s"\nFull step delta P = ${prevpop.flatValues.zip(finalpop.flatValues).map{case (p1,p2) => math.abs(p1-p2)}.sum}")

    MultiMacroState(
      time = state.time + 1,
      populations = currentPop,
      distanceMatrix = currentDist,
      modelStates = nextStates.toSeq
    )
  }

  def run(model: MultiMacroModel): MultiMacroResult = {
    import model._
    var currentState = MultiMacroState(0, initialStates.head.populations, initialStates.head.distanceMatrix, initialStates)
    val allStates = new ArrayBuffer[MultiMacroState]
    allStates.addOne(currentState)
    for(t <- 1 until finalTime){
      currentState = MultiMacroModel.nextStep(model, currentState).asInstanceOf[MultiMacroState]
      allStates.addOne(currentState)
    }
    MultiMacroResult(allStates.toSeq)
  }

}

