package org.openmole.spatialdata.scalatest.grid.synthetic

import org.openmole.spatialdata.test.TestMaths
import org.scalatest.flatspec.AnyFlatSpec

class CorrelatedPercolationGridGeneratorSpec extends AnyFlatSpec  {

  "Correlated field" should "have the correct properties" in {
    TestMaths.testFourierCorrelatedField(gui = false)
  }

}
