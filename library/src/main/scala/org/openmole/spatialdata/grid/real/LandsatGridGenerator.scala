package org.openmole.spatialdata.grid.real

import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.grid.GridGenerator

import scala.util.Random


/**
  * idea : wrap these tools : https://github.com/developmentseed/landsat-util
  * to generate landsat-based grids ?
  */
case class LandsatGridGenerator() extends GridGenerator {
  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = empty
}



object LandsatGridGenerator {

}
