package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.Matrix

trait FittedSpIntModel extends SpatialInteractionModel {
  def fit: SpatialInteractionModel => SpatialInteractionModel
  def fitted: SpatialInteractionModel = fit(this)
}


object FittedSpIntModel {

  //def poissonFit: SpatialInteractionModel => SpatialInteractionModel = _ => _

}
