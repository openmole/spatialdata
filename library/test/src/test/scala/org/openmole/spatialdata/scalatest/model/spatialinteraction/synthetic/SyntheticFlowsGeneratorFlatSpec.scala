package org.openmole.spatialdata.scalatest.model.spatialinteraction.synthetic

import org.openmole.spatialdata.test.TestSpatialInteraction
import org.scalatest.flatspec.AnyFlatSpec

class SyntheticFlowsGeneratorFlatSpec extends AnyFlatSpec {

  "Synthetic flows" should "be generated" in {
    TestSpatialInteraction.testSyntheticFlows()
  }

}
