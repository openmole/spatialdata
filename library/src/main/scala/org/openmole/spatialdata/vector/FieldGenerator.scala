package org.openmole.spatialdata.vector

import org.openmole.spatialdata.vector.SpatialField

import scala.util.Random

trait FieldGenerator[N] {
  def generateField(implicit rng: Random): SpatialField[N]
}
