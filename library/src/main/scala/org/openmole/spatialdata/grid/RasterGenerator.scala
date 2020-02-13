package org.openmole.spatialdata.grid

import org.openmole.spatialdata._

import scala.util.Random

trait RasterGenerator {

  def generateRaster(implicit rng: Random): RasterData[Double]

}
