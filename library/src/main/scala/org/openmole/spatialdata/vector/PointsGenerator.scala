package org.openmole.spatialdata.vector

import org.openmole.spatialdata.vector._

import scala.util.Random

trait PointsGenerator {

  /**
    * Q: object construction/size overhead when wrapping jts classes?
    * @param rng
    * @return
    */
  def generatePoints(implicit rng: Random): Points



}
