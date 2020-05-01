package org.openmole.spatialdata.scalatest.model.spatialinteraction

import org.openmole.spatialdata.test.TestSpatialInteraction
import org.scalatest.flatspec.AnyFlatSpec

class SinglyConstraintSpIntModelFlatSpec extends AnyFlatSpec {

  "Singly contraint spatial interaction model" should "converge" in {
    TestSpatialInteraction.testFitSinglyConstrainedSyntheticFlows()
  }

}
