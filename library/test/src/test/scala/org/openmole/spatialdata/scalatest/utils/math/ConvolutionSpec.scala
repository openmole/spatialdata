package org.openmole.spatialdata.scalatest.utils.math

import org.openmole.spatialdata.test.TestMaths
import org.scalatest.flatspec.AnyFlatSpec

class ConvolutionSpec extends AnyFlatSpec {

  "FFT Convolution" should "be correct" in {
    TestMaths.testConvolution()
  }

  "2D convolution" should "be correct" in {
    TestMaths.testConvolution2D()
  }


}
