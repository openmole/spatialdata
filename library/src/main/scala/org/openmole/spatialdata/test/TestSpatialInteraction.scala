package org.openmole.spatialdata.test

import org.openmole.spatialdata.model.spatialinteraction.synthetic.PolycentricGridGravityFlowsGenerator

import scala.util.Random

object TestSpatialInteraction {


  def testSyntheticFlows: Unit = {
    implicit val rng = new Random

    val syntheticFlows = PolycentricGridGravityFlowsGenerator(
      gridSize = 50,
      centers = 2,
      maxOrigin = 1.0,
      maxDestination = 1.0,
      originRadius = 10.0,
      destinationRadius = 8.0,
      originExponent = 1.5,
      destinationExponent = 1.5,
      costFunction = {d => math.exp(- d / 10.0)}
    ).generateFlows

  }


}
