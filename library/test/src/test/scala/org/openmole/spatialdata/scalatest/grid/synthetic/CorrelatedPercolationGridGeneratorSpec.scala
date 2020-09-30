package org.openmole.spatialdata.scalatest.grid.synthetic

import org.openmole.spatialdata.test.{TestMaths, TestSynthetic}
import org.scalatest.flatspec.AnyFlatSpec

class CorrelatedPercolationGridGeneratorSpec extends AnyFlatSpec  {

  "Correlated field" should "have the correct properties" in {
    TestMaths.testFourierCorrelatedField(gui = false)
  }

  "Correlated percolation grid" should "be generated" in {
    TestSynthetic.testCorrelatedPercolation()
  }

}
