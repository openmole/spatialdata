package org.openmole.spatialdata.scalatest.utils.io

import org.openmole.spatialdata.test.TestUtils
import org.scalatest.flatspec.AnyFlatSpec

class BinarySpec extends AnyFlatSpec {

  "Binary IO" should "not fail" in {
    TestUtils.testBinaryIO()
  }

}
