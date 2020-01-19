package org.openmole.spatialdata.grid

import org.openmole.spatialdata.grid.RasterLayerData

/**
  * Perturbate a grid
  */
trait GridPerturbator {

  def perturbateGrid(grid: RasterLayerData[Double]): RasterLayerData[Double]

}
