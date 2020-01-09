package org.openmole.spatialdata.scalatest.grid.synthetic

import org.openmole.spatialdata.Implicits._
import org.openmole.spatialdata.grid.synthetic.RandomGridGenerator
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class RandomGridGeneratorSpec extends AnyFlatSpec  {

  implicit val rng = new Random

  "A random grid" should "be a probability density" in {
    val randomGridGenerator = new RandomGridGenerator(50,totalPopulation = 1.0)
    val grid = randomGridGenerator.generateGrid
    assert(math.abs(grid.flatten.sum-1.0)<1e-10)
  }

  "An random grid size" should "be positive" in {
    assertThrows[IllegalArgumentException](new RandomGridGenerator(0).generateGrid)
  }

}
