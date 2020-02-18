package org.openmole.spatialdata.test

import org.openmole.spatialdata.model.spatialinteraction.SinglyConstrainedSpIntModel
import org.openmole.spatialdata.model.spatialinteraction.synthetic.PolycentricGridGravityFlowsGenerator
import org.openmole.spatialdata.utils.visualization

import scala.util.Random

object TestSpatialInteraction {


  def testSyntheticFlows: Unit = {
    implicit val rng = new Random

    val syntheticFlows = PolycentricGridGravityFlowsGenerator(
      gridSize = 50,
      centers = 2,
      maxOrigin = 1.0,
      maxDestination = 1.0,
      originRadius = 5.0,
      destinationRadius = 8.0,
      originExponent = 1.5,
      destinationExponent = 1.5,
      costFunction = {d => math.exp(- d / 15.0)}
    ).generateFlows

    // vis is bad - need some filtering + coords adjustement
    //visualization.staticFlowsVisualization(syntheticFlows)

    println(s"Avg synthetic flow = ${syntheticFlows.observedFlows.mean}")

    // try to fit singly constrained
    val model = SinglyConstrainedSpIntModel(syntheticFlows)
    val fittedModel = SinglyConstrainedSpIntModel.fitSinglyConstrainedSpIntModel(model)
    println(s"Fitted parameter = ${fittedModel.fittedParam}")

  }


}
