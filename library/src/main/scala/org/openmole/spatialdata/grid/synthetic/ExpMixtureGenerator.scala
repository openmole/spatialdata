
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
