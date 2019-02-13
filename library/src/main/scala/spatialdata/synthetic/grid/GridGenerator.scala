
package spatialdata.synthetic.grid

import spatialdata._

import scala.util.Random

/**
  * Generic generator of spatial grids
  *
  *
  *
  */
// FIXME this trait is used py package synthetic but also packages based on real data (osm) => rearchitecture !
trait GridGenerator {

  def generateGrid(implicit rng: Random): RasterLayerData[Double]

}



