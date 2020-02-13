package org.openmole.spatialdata.vector

import org.openmole.spatialdata.vector._

import scala.util.Random

trait PointsGenerator {

  def generatePoints(implicit rng: Random): Vector[Point]



}
