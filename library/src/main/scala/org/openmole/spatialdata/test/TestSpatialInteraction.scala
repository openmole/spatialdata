package org.openmole.spatialdata.test

import org.openmole.spatialdata.model.spatialinteraction.{SinglyConstrainedSpIntModel, SpatialInteractionModel}
import org.openmole.spatialdata.model.spatialinteraction.synthetic.PolycentricGridGravityFlowsGenerator
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Matrix
import org.openmole.spatialdata.utils.visualization

import scala.util.Random

object TestSpatialInteraction {


  def testFitSinglyConstrained: Unit = {
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


  /**
    * Dense faster than sparse for flow generation?
    */
  def testSyntheticFlows: Unit = {
    implicit val rng = new Random

    val syntheticFlowsGenerator = PolycentricGridGravityFlowsGenerator(
      gridSize = 50,
      centers = 5,
      maxOrigin = 1.0,
      maxDestination = 1.0,
      originRadius = 5.0,
      destinationRadius = 8.0,
      originExponent = 1.5,
      destinationExponent = 1.5,
      costFunction = {d => math.exp(- d / 15.0)}
    )

    // vis is bad - need some filtering + coords adjustement
    //visualization.staticFlowsVisualization(syntheticFlows)

    val (denseFlows,td) = utils.withTimer[Double,SpatialInteractionModel](_ => syntheticFlowsGenerator.generateFlows)(0.0)

    Matrix.MatrixImplementation.setDefaultSparse
    val sparseGen = syntheticFlowsGenerator.copy(sparse=true)
    val (sparseFlows,ts) = utils.withTimer[Double,SpatialInteractionModel](_ => sparseGen.generateFlows)(0.0)

    println(s"Avg synthetic dense flow = ${denseFlows.observedFlows.mean} ; t = $td")
    println(s"Avg synthetic sparse flow = ${sparseFlows.observedFlows.mean} ; t = $ts")

  }


}
