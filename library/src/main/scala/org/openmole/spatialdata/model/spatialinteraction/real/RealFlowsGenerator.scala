package org.openmole.spatialdata.model.spatialinteraction.real

import org.openmole.spatialdata.model.spatialinteraction.{FlowsGenerator, SpatialInteractionModel}

import scala.util.Random

case class RealFlowsGenerator(

                             ) extends FlowsGenerator {

  override def generateFlows(implicit rng: Random): SpatialInteractionModel = ???

}


object RealFlowsGenerator {

}

