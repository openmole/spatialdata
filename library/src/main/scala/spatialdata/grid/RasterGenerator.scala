
package spatialdata.grid

import spatialdata.RasterData

import scala.util.Random


trait RasterGenerator {

  def generateRaster(implicit rng: Random): RasterData[Double]

}

