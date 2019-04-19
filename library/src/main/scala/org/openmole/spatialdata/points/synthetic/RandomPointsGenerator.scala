package org.openmole.spatialdata.points.synthetic

import org.openmole.spatialdata._
import org.openmole.spatialdata.points.PointsGenerator

import scala.util.Random


/**
  * Random points
  * TODO add boundaries etc
  * @param npoints
  */
case class RandomPointsGenerator(npoints: Int) extends PointsGenerator {
  override def generatePoints(implicit rng: Random): Array[(Double, Double)] = RandomPointsGenerator.randomPoints(this)(rng)
}



object RandomPointsGenerator {

  def randomPoints(generator: RandomPointsGenerator)(implicit rng: Random): Array[Point2D] = Array.fill(generator.npoints){(rng.nextDouble(),rng.nextDouble())}

}
