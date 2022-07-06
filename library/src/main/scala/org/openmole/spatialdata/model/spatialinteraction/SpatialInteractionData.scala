package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.{EmptyMatrix, Matrix}
import org.openmole.spatialdata.vector.SpatialField

case class SpatialInteractionData(
                                   observedFlows: Matrix,
                                   distances: Matrix,
                                   originValues: SpatialField[Double],
                                   destinationValues: SpatialField[Double]
                                 ) extends SpatialInteractionModel {

  override def predictedFlows: Matrix = EmptyMatrix()

  /**
   * distance weights are not data
   * @return
   */
  override def distanceWeights: Matrix = EmptyMatrix()

}
