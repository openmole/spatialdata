package org.openmole.spatialdata.points

import org.openmole.spatialdata.{Point2D, SpatialField}

import scala.util.Random

trait PointsGenerator {

  def generatePoints(implicit rng: Random): Vector[Point2D]



}
