package org.openmole.spatialdata.utils.math

import org.openmole.spatialdata.grid.RasterDim

import scala.util.Random


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
    val w = worldSize match {case Left(l) => l; case Right((ww,_)) => ww}
    val h = worldSize match {case Left(l) => l; case Right((_,hh)) => hh}
    val vals = Array.fill(w,h)(0.0)
    val coords = centers match {
      case Left(i) => Seq.fill(i){(rng.nextInt(w),rng.nextInt(h))}
      case Right(c) => c
    }
    for(i<- 0 until w; j<- 0 until h){
      for(c <- coords){
        vals(i)(j) = vals(i)(j) + kernel(i - c._1,j - c._2)
      }
    }
    //array to seq
    //Seq.tabulate(w,h){(i:Int,j:Int)=>(vals(i)(j),(i,j))}
    vals
  }

}

