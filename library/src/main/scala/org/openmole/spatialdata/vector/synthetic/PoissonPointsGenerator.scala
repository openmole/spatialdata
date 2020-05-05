package org.openmole.spatialdata.vector.synthetic

import org.openmole.spatialdata.utils.math.Stochastic.PoissonDistribution
import org.openmole.spatialdata.vector.{Point, Points, PointsGenerator}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


case class PoissonPointsGenerator(
                                   lambda: Double,
                                   lambdaField: Array[Array[Double]] = Array.empty,
                                   homogenous: Boolean = true,
                                   xmin: Double = 0.0,
                                   xmax: Double = 1.0,
                                   ymin: Double = 0.0,
                                   ymax: Double = 1.0
                                 ) extends PointsGenerator {
  override def generatePoints(implicit rng: Random): Points = Points.fromPoints(
    if (homogenous) PoissonPointsGenerator.homogenousPoissonPoints(this) else PoissonPointsGenerator.heterogenousPoissonPoints(this)
  )

  def area: Double = (xmax - xmin)*(ymax - ymin)

  def weightedArea: Double = if (lambdaField.length==0) 0.0 else
    lambdaField.flatten.sum * (xmax - xmin)*(ymax - ymin) / (lambdaField.length*lambdaField(0).length)
}


object PoissonPointsGenerator {

  def apply(lambda: Double,xmin: Double, xmax: Double, ymin: Double, ymax: Double): PoissonPointsGenerator = PoissonPointsGenerator(lambda,Array.empty,homogenous = true,xmin,xmax,ymin,ymax)

  def apply(lambdaField: Array[Array[Double]]): PoissonPointsGenerator = PoissonPointsGenerator(0.0,lambdaField,homogenous = false)

  def apply(lambdaField: Array[Array[Double]], xmin: Double, xmax: Double, ymin: Double, ymax: Double): PoissonPointsGenerator =
    PoissonPointsGenerator(0.0,lambdaField,homogenous = false,xmin,xmax,ymin,ymax)

  /**
    * Homogenous poisson point process
    * https://en.wikipedia.org/wiki/Poisson_point_process
    *
    * @param generator generator
    * @param rng rng
    * @return
    */
  def homogenousPoissonPoints(generator: PoissonPointsGenerator)(implicit rng: Random): Vector[Point] = {
    // draw number of points
    val n = PoissonDistribution(generator.lambda*generator.area).draw.toInt
    // then just random points
    Vector.fill(n){(generator.xmin + (generator.xmax - generator.xmin)*rng.nextDouble(),
      generator.ymin + (generator.ymax - generator.ymin)*rng.nextDouble())}
  }

  /**
    * Heterogenous poisson point process
    *
    * intensity field represented by a raster
    *
    * @param generator generator
    * @param rng rng
    * @return
    */
  def heterogenousPoissonPoints(generator: PoissonPointsGenerator)(implicit rng: Random): Vector[Point] = {
    val totalIntensity = generator.weightedArea
    val n = PoissonDistribution(totalIntensity).draw.toInt
    // rejection sampling ; the array representing intensity must be inversed on rows
    val lambdas: Array[Array[Double]] = generator.lambdaField.reverse
    val points = new ArrayBuffer[(Double,Double)]
    while (points.length < n){
      val (x,y) = (rng.nextDouble(),rng.nextDouble())
      val (i,j) = (math.floor(y*lambdas.length).toInt,math.floor(x*lambdas(0).length).toInt)
      val localIntensity: Double = lambdas(i)(j)
      if (localIntensity / totalIntensity > rng.nextDouble()) points.append((generator.xmin + (generator.xmax - generator.xmin)*x,generator.ymin + (generator.ymax - generator.ymin)*y))
    }
    points.toVector
  }


}
