package org.openmole.spatialdata.scalatest.vector.measures

import org.openmole.spatialdata.test.TestIndicators
import org.scalatest.flatspec.AnyFlatSpec

class SummarySpatialStatisticsSpec extends AnyFlatSpec {

  "Summary spatial statistics" should "not fail" in {
    TestIndicators.testSpatialIndics()
  }

}
