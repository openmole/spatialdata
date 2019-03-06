
package spatialdata.synthetic.grid

import spatialdata.grid.GridGenerator
import spatialdata.{RasterDim, RasterLayerData}

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
                          kernelRadius: Double

                        ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = {
    println("Exp mixture grid of size "+size+" ; "+centers+" ; "+maxValue+" ; "+kernelRadius)
    def expKernel(x: Double, y: Double): Double = maxValue*exp(-sqrt(pow(x,2.0)+pow(y,2.0))/kernelRadius)
    KernelMixture.kernelMixture(size,Left(centers),expKernel,rng)
  }


}




object KernelMixture {

  def kernelMixture(worldSize: RasterDim,
                    centers: Either[Int,Seq[Seq[Int]]],
                    kernel: (Double,Double)=>Double,
                    rng: Random
                   ): Array[Array[Double]] //Seq[Seq[(Double,(Int,Int))]]
    = {
    //val vals = Seq.fill(worldSize,worldSize)(0.0)
    val w = worldSize match {case Left(l) => l; case Right((w,_)) => w}
    val h = worldSize match {case Left(l) => l; case Right((_,h)) => h}
    val vals = Array.fill(w,h)(0.0)
    val coords = centers match {
      case Left(i) => Seq.fill(i){Seq(rng.nextInt(w),rng.nextInt(h))}
      case Right(c) => c
    }
    for(i<- 0 to w-1; j<- 0 to h-1){
      for(c <- coords){
        vals(i)(j) = vals(i)(j) + kernel((i - c(0)),(j - c(1)))
      }
    }
    //array to seq
    //Seq.tabulate(w,h){(i:Int,j:Int)=>(vals(i)(j),(i,j))}
    vals
  }

}
