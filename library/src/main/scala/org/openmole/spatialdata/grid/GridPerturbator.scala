package org.openmole.spatialdata.grid

import org.openmole.spatialdata.RasterLayerData

/**
  * Perturbate a grid
  */
trait GridPerturbator {

  def perturbateGrid(grid: RasterLayerData[Double]): RasterLayerData[Double]

}
