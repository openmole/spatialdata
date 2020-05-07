package org.openmole.spatialdata.scalatest.grid.measures

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.synthetic.RandomGridGenerator
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class GridMorphologySpec extends AnyFlatSpec {


  implicit val rng: Random = new Random

  "Convolution" should "yield same results as direct" in {
    val grid = RandomGridGenerator(10).generateGrid
    assert(math.abs(GridMorphology.moran(grid) - GridMorphology.moranDirect(grid))<1e-5,"Moran convolution and direct differ")
    assert(math.abs(GridMorphology.distanceMean(grid) - GridMorphology.distanceMeanDirect(grid))<1e-5,"Distance mean convolution and direct differ")
  }




}
