package spatialdata

object SpatialSampling {
  /**
    * Generate a sample of random grids
    * @param gridSize
    * @param samples
    * @param rng
    * @return
    */
  def randomGridSample(gridSize: Int, samples: Int, rng: scala.util.Random) = Array.fill(samples, gridSize, gridSize) { rng.nextDouble() }

  /**
    * TODO :
    *   - other configs : cf ecology ?
    */
}
