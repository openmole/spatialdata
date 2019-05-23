
package org.openmole.spatialdata.grid.synthetic

import org.openmole.spatialdata.grid.GridGenerator

import org.openmole.spatialdata._

import scala.util.Random
import math._


case class ExpMixtureGenerator(
                          /**
                            * size
                            */
                        size: RasterDim,

                          /**
                            * Number of centers
                            */
                          centers: Int,

                          /**
                            * Value of the exp at 0
                            */
                          maxValue: Double,

                          /**
                            * Radius of the exp kernel
                            */
                          kernelRadius: Double,

                          /**
                            * Should the distribution be normalized
                            */
                              // TODO not really useful ?
                          normalized: Boolean = false,

                          centerCoordinates: Seq[Point2D] = Seq.empty

                        ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = {
    //println("Exp mixture grid of size "+size+" ; "+centers+" ; "+maxValue+" ; "+kernelRadius)
    def expKernel(x: Double, y: Double): Double = maxValue*exp(-sqrt(pow(x,2.0)+pow(y,2.0))/kernelRadius)
    val eithcenters = centerCoordinates.size match {case 0 => Left(centers);case _ => Right(centerCoordinates.map(c => (c._1.toInt,c._2.toInt)))}
    KernelMixture.kernelMixture(size,eithcenters,expKernel,rng)
  }


}


/**
  * TODO include more realistic kernels for cities - see Chen, Y. (2010). A new model of urban population density indicating latent fractal structure. International Journal of Urban Sustainable Development, 1(1-2), 89-110.
  */
object KernelMixture {

  def kernelMixture(worldSize: RasterDim,
                    centers: Either[Int,Seq[(Int,Int)]],
                    kernel: (Double,Double)=>Double,
                    rng: Random
                   ): Array[Array[Double]] //Seq[Seq[(Double,(Int,Int))]]
    = {
    //val vals = Seq.fill(worldSize,worldSize)(0.0)
    val w = worldSize match {case Left(l) => l; case Right((w,_)) => w}
    val h = worldSize match {case Left(l) => l; case Right((_,h)) => h}
    val vals = Array.fill(w,h)(0.0)
    val coords = centers match {
      case Left(i) => Seq.fill(i){(rng.nextInt(w),rng.nextInt(h))}
      case Right(c) => c
    }
    for(i<- 0 to w-1; j<- 0 to h-1){
      for(c <- coords){
        vals(i)(j) = vals(i)(j) + kernel((i - c._1),(j - c._2))
      }
    }
    //array to seq
    //Seq.tabulate(w,h){(i:Int,j:Int)=>(vals(i)(j),(i,j))}
    vals
  }

}





object ExponentialMixture {

  /**
    * Generate one exponential kernel mixture grid
    * -- DEPRECATED, function below is more general --
    *
    * @param gridSize
    * @param nCenters
    * @param maxValue
    * @param kernelRadius
    * @param rng
    * @return
    */
  def expMixtureGrid1D(gridSize: Int, nCenters: Int, maxValue: Double, kernelRadius: Double, rng: scala.util.Random): RasterLayerData[Double] = {
    val arrayVals = Array.fill[Double](gridSize, gridSize) {
      0.0
    }
    val centers = Array.fill[Int](nCenters, 2) {
      rng.nextInt(gridSize)
    }
    for (i ← 0 to gridSize - 1; j ← 0 to gridSize - 1) {
      for (c ← 0 to nCenters - 1) {
        arrayVals(i)(j) = arrayVals(i)(j) + maxValue * math.exp(-math.sqrt(math.pow((i - centers(c)(0)), 2) + math.pow((j - centers(c)(1)), 2)) / kernelRadius)
      }
    }
    arrayVals
  }

  /**
    * A multilayer exponential mixture with same centers
    *
    * @param gridSize
    * @param nCenters
    * @param maxValues
    * @param kernelRadius
    * @param rng
    * @return
    */
  def expMixtureGridSameCenters(
                                 gridSize:     Either[Int, (Int, Int)],
                                 nCenters:     Int,
                                 maxValues:    Either[Double, Seq[Double]],
                                 kernelRadius: Either[Double, Seq[Double]],
                                 rng:          scala.util.Random
                               ): (RasterData[Double], Seq[Point2D]) = {
    // grid dimensions
    val dims: (Int, Int) = gridSize match {
      case Left(s)  ⇒ (s, s)
      case Right(d) ⇒ d
    }

    // ensure parameters consistency
    val maxVals = maxValues match {
      case Left(d)   ⇒ Seq(d)
      case Right(dd) ⇒ dd
    }
    val radiuses = kernelRadius match {
      case Left(d)   ⇒ Seq(d)
      case Right(dd) ⇒ dd
    }

    //if (maxVals.size != radiuses.size) throw new UserBadDataError("Wrong input parameters")

    val layerdim = maxVals.size

    // generate centers
    val centers = Seq.fill[Point2D](nCenters) {
      (rng.nextInt(dims._1).toDouble, rng.nextInt(dims._2).toDouble)
    }

    // fill the empty raster
    val raster = Seq.fill[RasterLayerData[Double]](layerdim) {
      Array.fill(dims._1, dims._2)(0.0)
    }

    for (k ← 0 to layerdim - 1; i ← 0 to dims._1 - 1; j ← 0 to dims._2 - 1; c ← 0 to nCenters - 1) {
      raster(k)(i)(j) = raster(k)(i)(j) + maxVals(k) * math.exp(-math.sqrt(math.pow((i - centers(c)._1), 2) + math.pow((j - centers(c)._2), 2)) / radiuses(k))
    }
    (raster, centers)
  }

  /**
    * Generate a sample of exponent kernel mixture grids
    *
    * @param samples
    * @param gridSize
    * @param nCenters
    * @param maxValue
    * @param kernelRadius
    * @param rng
    */
  def expMixtureGridSameCentersSample(
                                       samples:      Int,
                                       gridSize:     Either[Int, (Int, Int)],
                                       nCenters:     Int                         = 1,
                                       maxValue:     Either[Double, Seq[Double]] = Left(1.0),
                                       kernelRadius: Either[Double, Seq[Double]] = Left(1.0),
                                       rng:          scala.util.Random
                                     ): Seq[RasterData[Double]] = {
    Seq.fill(samples) {
      expMixtureGridSameCenters(gridSize, nCenters, maxValue, kernelRadius, rng)._1
    }
  }

}

