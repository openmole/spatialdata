
package org.openmole.spatialdata.grid.synthetic

import org.openmole.spatialdata._
import org.openmole.spatialdata.Implicits._
import org.openmole.spatialdata.grid.GridGenerator

import scala.util.Random


case class RandomGridGenerator(
                              /**
                               * The size of generated grids
                               */
                                size : RasterDim,

                              /**
                                * Rescaling factor (by default a probability density)
                                */
                              totalPopulation: Double = -1.0,

                              /**
                                * approximate proportion of empty cells
                                */
                              occupiedCells: Double = -1.0,

                              /**
                                * Number of layers
                                */
                              layers : Int = 1
                              ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = {
    val grid = RandomGridGenerator.randomGrid(size,occupiedCells,rng)
    totalPopulation match {
      case -1 => grid
      case _ => {val s = grid.flatten.sum; grid.map{_.map{_ * totalPopulation / s }}}
    }
  }

}


object RandomGridGenerator {

  def apply(size: Int): RandomGridGenerator = new RandomGridGenerator(size)

  /**
    * proba density with occupied cells
    * @param size
    * @param occupiedCells
    * @return
    */
  def apply(size: Int,occupiedCells: Double): RandomGridGenerator = new RandomGridGenerator(Left(size),1.0,occupiedCells,1)


  /**
    * Random layer
    *
    * @param size
    * @param samples
    * @param rng
    * @return
    */
  def randomGrid(size: RasterDim, occupied: Double, rng: Random) : RasterLayerData[Double] = {
    def randompop: Double = occupied match {
      case -1 => rng.nextDouble()
      case _ => if(rng.nextDouble()<occupied) rng.nextDouble() else 0.0
    }
    size match {
      case Left(s) if s <= 0 => throw new IllegalArgumentException("Raster size should be positive")
      case Left(s)=>Array.fill(s, s)(randompop)
      case Right((w,h)) if w <= 0 || h <= 0 => throw new IllegalArgumentException("Raster size should be positive")
      case Right((w,h))=>Array.fill(w, h)(randompop)
    }
  }

  /**
    * Generate a sample of random grids
    *
    * @param gridSize
    * @param samples
    * @param rng
    * @return
    */
  def randomGridSample(gridSize: Int, samples: Int, rng: scala.util.Random) = Array.fill(samples, gridSize, gridSize) {
    rng.nextDouble()
  }


}