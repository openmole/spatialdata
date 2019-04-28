package org.openmole.spatialdata.points.synthetic

import org.openmole.spatialdata._
import org.openmole.spatialdata.points.PointsGenerator

import scala.util.Random


case class PoissonPointsGenerator(lambda: Double) extends PointsGenerator {
  override def generatePoints(implicit rng: Random): Array[(Double, Double)] = PoissonPointsGenerator.homogenousPoissonPoints(lambda)
}


object PoissonPointsGenerator {

  /**
    *
    * https://en.wikipedia.org/wiki/Poisson_point_process
    *
    * @param lambda
    * @param rng
    * @return
    */
  def homogenousPoissonPoints(lambda: Double)(implicit rng: Random) = Array.empty[Point2D]

}
