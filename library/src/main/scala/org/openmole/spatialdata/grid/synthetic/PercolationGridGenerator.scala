
package org.openmole.spatialdata.grid.synthetic

import org.openmole.spatialdata._
import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.network.synthetic.PercolationNetworkGenerator

import scala.util.Random


/**
  *
  * Rasterization of a grid network percolation, described in
  *   Raimbault and Perret, 2019. Generating urban morphologies at large scales. Artificial Life Conference Proceedings 2019 NO. 31, 179-186
  *
  * @param size grid size
  * @param percolationProba percolation probability
  * @param bordPoints number of bord points to connect
  * @param linkwidth link width
  * @param maxIterations max number of iterations
  * @param percolateBuildings does the percolated stuff corresponds to buildings or streets ?
  */
case class PercolationGridGenerator(
                                   size: Int,
                                   percolationProba: Double,
                                   bordPoints: Int,
                                   linkwidth: Double,
                                   maxIterations: Int,
                                   percolateBuildings: Boolean = false
                                   ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = {
    //println("Percolation grid of size "+size+" ; "+percolationProba+" ; "+bordPoints+" ; "+linkwidth)
    val percolatedGrid = network.networkToGrid(PercolationNetworkGenerator(size,percolationProba,bordPoints,linkwidth,maxIterations).generateNetwork(rng),linkwidth=linkwidth)
    if (percolateBuildings) percolatedGrid else percolatedGrid.map{_.map{1.0 - _}}
  }

}

object PercolationGridGenerator


