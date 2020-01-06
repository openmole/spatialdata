package org.openmole.spatialdata.vector.synthetic

import org.openmole.spatialdata.{Point2D, RasterLayerData}
import org.openmole.spatialdata.vector.PointsGenerator
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random

case class DensityPointsGenerator(
                                  npoints: Int,
                                  densityGrid: RasterLayerData[Double],
                                  xmin: Double = 0.0,
                                  xmax: Double = 1.0,
                                  ymin: Double = 0.0,
                                  ymax: Double = 1.0
                                ) extends PointsGenerator {
  override def generatePoints(implicit rng: Random): Vector[Point2D] = DensityPointsGenerator.densityPoints(this)(rng)
}



object DensityPointsGenerator {

  def apply(npoints: Int, densityGrid: RasterLayerData[Double],normalize: Boolean): DensityPointsGenerator = {
    val s = densityGrid.flatten.sum
    val normalized = if(s==1.0) densityGrid else {densityGrid.map{_.map{_ / s}}}
    DensityPointsGenerator(npoints,normalized,0.0,1.0,0.0,1.0)
  }

  def densityPoints(generator: DensityPointsGenerator)(implicit rng: Random): Vector[Point2D] = {
    val flatGrid: Array[(Int, Int, Double)] = generator.densityGrid.zipWithIndex.flatMap { c: (Array[Double], Int) => c._1.zipWithIndex.map { case (r, j) => (c._2, j, r) } }
    Stochastic.sampleWithReplacementBy[(Int, Int, Double)](
      flatGrid,
      _._3,
      generator.npoints
    ).map { case (i, j, _) =>
      val (rowstep, colstep) = (1.0 / generator.densityGrid.length.toDouble, 1.0 / generator.densityGrid(0).length.toDouble)
      ((i.toDouble + rng.nextDouble()) * rowstep, (j.toDouble + rng.nextDouble()) * colstep)
    }
  }

}


