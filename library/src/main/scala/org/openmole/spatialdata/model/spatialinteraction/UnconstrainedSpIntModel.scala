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
                                  originValues: SpatialField,
                                  destinationValues: SpatialField,
                                  predictedFlows: Matrix,
                                  fit: SpatialInteractionModel => SpatialInteractionModel = {s: SpatialInteractionModel => s}//FittedSpIntModel.poissonFit
                                  ) extends FittedSpIntModel {

}

object UnconstrainedSpIntModel {

}
