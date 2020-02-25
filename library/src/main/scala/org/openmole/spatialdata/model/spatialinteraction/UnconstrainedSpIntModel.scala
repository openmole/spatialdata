package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.Matrix
import org.openmole.spatialdata.vector.SpatialField

/**
  * rq: the difference to constrained / unconstrained model is the regression specification ?
  * @param observedFlows
  * @param distances
  * @param originValues
  * @param destinationValues
  * @param predictedFlows
  * @param fit
  */
case class UnconstrainedSpIntModel(
                                  observedFlows: Matrix,
                                  distances: Matrix,
                                  originValues: SpatialField[Double],
                                  destinationValues: SpatialField[Double],
                                  predictedFlows: Matrix
                                  ) extends SpatialInteractionModel {

}

object UnconstrainedSpIntModel {

}
