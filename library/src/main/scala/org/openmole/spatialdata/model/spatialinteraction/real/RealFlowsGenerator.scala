package org.openmole.spatialdata.model.spatialinteraction.real

import org.openmole.spatialdata.model.spatialinteraction.{FlowsGenerator, SpatialInteractionModel}
import org.openmole.spatialdata.utils.math.SparseMatrix.SparseMatrixImplementation

import scala.util.Random

case class RealFlowsGenerator(

                             ) extends FlowsGenerator {

  override def generateFlows(implicit rng: Random): SpatialInteractionModel = ???

}


object RealFlowsGenerator {

}

