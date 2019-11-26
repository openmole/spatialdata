package org.openmole.spatialdata.points

import org.openmole.spatialdata.SpatialField

import scala.util.Random

trait FieldGenerator {
  def generateField(implicit rng: Random): SpatialField
}
