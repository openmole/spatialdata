package spatialdata.grid

import spatialdata.RasterLayerData

import scala.util.Random

/**
  * Generic generator of spatial grids
  * (single layer ~ RasterLayerData)
  */
trait GridGenerator {

  def generateGrid(implicit rng: Random): RasterLayerData[Double]

}
