package org.openmole.spatialdata.scalatest.utils.math

import org.openmole.spatialdata.test.TestMatrix
import org.scalatest.flatspec.AnyFlatSpec

class MatrixSpec extends AnyFlatSpec {

  "Sparse matrix implementations" should "be comparable" in {
    TestMatrix.testImplementations
  }

}