package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.Matrix

trait FittedSpIntModel extends SpatialInteractionModel {
  def fit: SpatialInteractionModel => SpatialInteractionModel
}


object FittedSpIntModel {

  def poissonFit: SpatialInteractionModel => SpatialInteractionModel = _ => _

}
