
package spatialdata.synthetic.grid

import spatialdata._

import scala.util.Random


case class RandomGridGenerator(
                              /**
                               * The size of generated grids
                               */
                                size : RasterDim,

                              /**
                                * Number of layers
                                */
                              layers : Int = 1
                              ) extends GridGenerator {

  override def generateGrid[Double](size: RasterDim)(implicit rng: Random): RasterLayerData[Double] = RandomGridGenerator.randomGrid(size,rng).asInstanceOf[RasterLayerData[Double]]

}


object RandomGridGenerator {


  implicit def rng: Random = new Random

  /**
    * Random layer
    *
    * @param size
    * @param samples
    * @param rng
    * @return
    */
  def randomGrid(size: RasterDim, rng: Random) : RasterLayerData[Double] = {
    size match {
      case Left(size)=>Array.fill(size, size){ rng.nextDouble() }
      case Right((w,h))=>Array.fill(w, h){ rng.nextDouble() }
    }
  }


}