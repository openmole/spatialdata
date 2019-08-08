package org.openmole.spatialdata.points.synthetic

import org.openmole.spatialdata._
import org.openmole.spatialdata.points.PointsGenerator

import scala.util.Random


/**
  * Random points
  *
  * @param npoints
  */
case class RandomPointsGenerator(
                                  npoints: Int,
                                  xmin: Double = 0.0,
                                  xmax: Double = 1.0,
                                  ymin: Double = 0.0,
                                  ymax: Double = 1.0
                                ) extends PointsGenerator {
  override def generatePoints(implicit rng: Random): Array[(Double, Double)] = RandomPointsGenerator.randomPoints(this)(rng)
}



object RandomPointsGenerator {

  def randomPoints(generator: RandomPointsGenerator)(implicit rng: Random): Array[Point2D] =
    Array.fill(generator.npoints){(generator.xmin + (generator.xmax - generator.xmin)*rng.nextDouble(),
                                    generator.ymin + (generator.ymax - generator.ymin)*rng.nextDouble())}

}
