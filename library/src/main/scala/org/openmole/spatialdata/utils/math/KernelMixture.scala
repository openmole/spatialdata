package org.openmole.spatialdata.utils.math

import org.openmole.spatialdata.grid.RasterDim

import scala.util.Random


/**
  * Future work
  * include more realistic kernels for cities - see Chen, Y. (2010). A new model of urban population density indicating latent fractal structure. International Journal of Urban Sustainable Development, 1(1-2), 89-110.
  */
object KernelMixture {

  def kernelMixture(worldSize: RasterDim,
                    centers: Either[Int,Seq[(Int,Int)]],
                    kernels: Seq[(Double,Double)=>Double],
                    rng: Random
                   ): Array[Array[Double]]
  = {
    val w = worldSize match {case Left(l) => l; case Right((ww,_)) => ww}
    val h = worldSize match {case Left(l) => l; case Right((_,hh)) => hh}
    val vals = Array.fill(w,h)(0.0)
    val coords = centers match {
      case Left(i) => Seq.fill(i){(rng.nextInt(w),rng.nextInt(h))}
      case Right(c) => c
    }
    for(i<- 0 until w; j<- 0 until h){
      for(c <- coords.indices){
        def k(x: Double, y: Double): Double = if (kernels.size==1) kernels(0)(x,y) else kernels(c)(x,y)
        vals(i)(j) = vals(i)(j) + k(i - coords(c)._1,j - coords(c)._2)
      }
    }
    vals
  }

}

