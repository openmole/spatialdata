package org.openmole.spatialdata.vector.real

import org.openmole.spatialdata.vector.SpatialField

import scala.util.Random

trait SpatialFieldPerturbator {

  def perturbateSpatialField(field: SpatialField[Double])(implicit rng: Random): SpatialField[Double]

}
