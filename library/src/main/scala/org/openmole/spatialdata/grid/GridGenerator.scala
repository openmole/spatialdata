package org.openmole.spatialdata.grid

import org.openmole.spatialdata._

import scala.util.Random

/**
  * Generic generator of spatial grids
  * (single layer ~ RasterLayerData)
  *
  * TODO implement fractal + kernels proposed in Hong I, Jung WS, Jo HH (2019) Gravity model explained by the radiation model on a population landscape. PLOS ONE 14(6): e0218028
  *
  *
  */
trait GridGenerator {

  def generateGrid(implicit rng: Random): RasterLayerData[Double]

}
