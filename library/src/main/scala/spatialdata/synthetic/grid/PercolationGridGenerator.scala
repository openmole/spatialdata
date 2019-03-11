
package spatialdata.synthetic.grid

import spatialdata.RasterLayerData
import spatialdata.grid.GridGenerator
import spatialdata.synthetic.network.PercolationNetworkGenerator
import spatialdata.network._

import scala.util.Random


case class PercolationGridGenerator(
                                   size: Int,
                                   percolationProba: Double,
                                   bordPoints: Int,
                                   linkwidth: Double,

                                   /**
                                     * does the percolated stuff corresponds to buildings or streets ?
                                     */
                                   percolateBuildings: Boolean = false
                                   ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = {
    println("Percolation grid of size "+size+" ; "+percolationProba+" ; "+bordPoints+" ; "+linkwidth)
    val percolatedGrid = Network.networkToGrid(PercolationNetworkGenerator(size,percolationProba,bordPoints,linkwidth).generateNetwork(rng),linkwidth=linkwidth)
    if (percolateBuildings) percolatedGrid else percolatedGrid.map{_.map{1.0 - _}}
  }

}

object PercolationGridGenerator


