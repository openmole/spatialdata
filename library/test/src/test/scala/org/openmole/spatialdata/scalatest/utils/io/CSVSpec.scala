package org.openmole.spatialdata.scalatest.utils.io

import org.openmole.spatialdata.test.TestUtils
import org.scalatest.flatspec.AnyFlatSpec

class CSVSpec extends AnyFlatSpec {

  "CSV IO" should "not fail" in {
    TestUtils.testCSV()
  }

}
