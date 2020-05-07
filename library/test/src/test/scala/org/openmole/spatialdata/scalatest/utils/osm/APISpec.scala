package org.openmole.spatialdata.scalatest.utils.osm

import org.openmole.spatialdata.test.TestOSM
import org.scalatest.flatspec.AnyFlatSpec

class APISpec extends AnyFlatSpec{

  "Overpass" should "work" in {
    TestOSM.testOverpass()
  }

  "Building extractor" should "work" in {
    TestOSM.testBuildingExtractor()
  }

  // need a dummy test data for layer sampling

}
