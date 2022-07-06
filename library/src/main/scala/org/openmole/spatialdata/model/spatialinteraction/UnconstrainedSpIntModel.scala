package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.{Matrix, EmptyMatrix}
import org.openmole.spatialdata.vector.SpatialField

/**
  * rq: the difference to constrained / unconstrained model is the regression specification
 *    when viewing them as statistical models
 *     -> implement stat approach?; compare with iterative approach?
 *
  * @param observedFlows observedFlows
  * @param distances distances
  * @param originValues originValues
  * @param destinationValues destinationValues
  * @param predictedFlows predictedFlows
  */
case class UnconstrainedSpIntModel(
                                  observedFlows: Matrix,
                                  distances: Matrix,
                                  originValues: SpatialField[Double],
                                  destinationValues: SpatialField[Double],
                                  predictedFlows: Matrix
                                  ) extends SpatialInteractionModel {

  override def distanceWeights: Matrix = EmptyMatrix()

}

object UnconstrainedSpIntModel {

}
