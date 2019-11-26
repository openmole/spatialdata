package org.openmole.spatialdata.points.real

import org.openmole.spatialdata.SpatialField

import scala.util.Random

trait SpatialFieldPerturbator {

  def perturbateSpatialField(field: SpatialField)(implicit rng: Random): SpatialField

}
