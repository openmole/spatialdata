
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
                                   linkwidth: Double
                                   ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = Network.networkToGrid(PercolationNetworkGenerator(size,percolationProba,bordPoints,linkwidth).generateNetwork(rng),linkwidth=linkwidth)

}



