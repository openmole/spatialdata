
package spatialdata.synthetic.grid

import spatialdata._

import scala.util.Random

/**
  * Generic generator of spatial grids
  */
trait GridGenerator {

  def generateGrid(implicit rng: Random): RasterLayerData[Double]

}



