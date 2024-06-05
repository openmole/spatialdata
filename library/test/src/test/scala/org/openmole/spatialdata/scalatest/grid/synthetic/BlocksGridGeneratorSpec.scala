package org.openmole.spatialdata.scalatest.grid.synthetic

import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.synthetic.BlocksGridGenerator
import org.openmole.spatialdata.utils.visualization
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BlocksGridGeneratorSpec extends AnyFlatSpec  {

    implicit val rng: Random = new Random

  //FIXME check the components function for grids
  /*
    "A block grid" should "have less components than specified blocks" in {
      val numblocks = 5
      val grid = new BlocksGridGenerator(50,numblocks,5,10).generateGrid
      visualization.staticRasterVisualization(grid)
      assert(GridMorphology.components(grid)<=numblocks)
    }
*/

}
