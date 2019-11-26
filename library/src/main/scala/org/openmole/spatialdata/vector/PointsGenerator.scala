package org.openmole.spatialdata.vector

import org.openmole.spatialdata.{Point2D, SpatialField}

import scala.util.Random

trait PointsGenerator {

  def generatePoints(implicit rng: Random): Vector[Point2D]



}
