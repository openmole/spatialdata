package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.Matrix
import org.openmole.spatialdata.vector.SpatialField

trait SpatialInteractionModel {

  def observedFlows: Matrix
  def distances: Matrix
  def originValues: SpatialField
  def destinationValues: SpatialField

  def predictedFlows: Matrix

}

case class SpatialInteractionModel()
