package org.openmole.spatialdata.scalatest.utils.io

import org.openmole.spatialdata.test.TestUtils
import org.scalatest.flatspec.AnyFlatSpec

class GeoPackageSpec extends AnyFlatSpec {

  "GeoPackage input" should "not fail" in {
    TestUtils.testGeopackageInput()
  }

}
