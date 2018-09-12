
package spatialdata.synthetic.grid

import spatialdata._

import scala.util.Random

/**
  * Generic generator of spatial grids
  */
trait GridGenerator {

  def generateGrid[N](size: RasterDim)(implicit rng: Random): RasterLayerData[N]

}



