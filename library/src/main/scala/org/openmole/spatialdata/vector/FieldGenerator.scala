package org.openmole.spatialdata.vector

import org.openmole.spatialdata.vector.SpatialField

import scala.util.Random

trait FieldGenerator {
  def generateField(implicit rng: Random): SpatialField
}
