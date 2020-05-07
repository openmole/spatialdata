package org.openmole.spatialdata.scalatest.vector

import org.openmole.spatialdata.scalatest.vector.measures.VectorMeasuresSuite
import org.scalatest.Suites

class VectorSuite extends Suites(
  new VectorMeasuresSuite
)
