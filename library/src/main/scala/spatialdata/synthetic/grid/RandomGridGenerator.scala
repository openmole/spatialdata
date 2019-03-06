
package spatialdata.synthetic.grid

import spatialdata._
import spatialdata.grid.GridGenerator

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

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = RandomGridGenerator.randomGrid(size,rng)

}


object RandomGridGenerator {

  def apply(size: Int): RandomGridGenerator = new RandomGridGenerator(size)


  /**
    * Random layer
    *
    * @param size
    * @param samples
    * @param rng
    * @return
    */
  def randomGrid(size: RasterDim, rng: Random) : RasterLayerData[Double] = {
    println("Random grid")
    size match {
      case Left(size)=>Array.fill(size, size){ rng.nextDouble() }
      case Right((w,h))=>Array.fill(w, h){ rng.nextDouble() }
    }
  }


}