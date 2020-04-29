package org.openmole.spatialdata.scalatest.utils.math

import org.openmole.spatialdata.test.TestMaths
import org.scalatest.flatspec.AnyFlatSpec

class StochasticSpec extends AnyFlatSpec {

  "Stochastic distributions" should "not yield NaNs" in {
    TestMaths.testStochastic()
  }

}
