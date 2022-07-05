package org.openmole.spatialdata.vector.synthetic

import org.openmole.spatialdata.vector._
import org.openmole.spatialdata.vector.PointsGenerator

import scala.util.Random


/**
  * Random points
  *
  * @param npoints number of points
  */
case class RandomPointsGenerator(
                                  npoints: Int,
                                  xmin: Double = 0.0,
                                  xmax: Double = 1.0,
                                  ymin: Double = 0.0,
                                  ymax: Double = 1.0
                                ) extends PointsGenerator {
  override def generatePoints(implicit rng: Random): Points = Points.fromPoints(RandomPointsGenerator.randomPoints(this)(rng))
}



object RandomPointsGenerator {

  def randomPoints(generator: RandomPointsGenerator)(implicit rng: Random): Vector[Point] =
    //Array.fill(generator.npoints){(math.digits(generator.xmin + (generator.xmax - generator.xmin)*rng.nextDouble,4),
    //  math.digits(generator.ymin + (generator.ymax - generator.ymin)*rng.nextDouble,4))}
    Vector.fill(generator.npoints){(generator.xmin + (generator.xmax - generator.xmin)*rng.nextDouble(),
      generator.ymin + (generator.ymax - generator.ymin)*rng.nextDouble())}
}
