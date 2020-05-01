package org.openmole.spatialdata.scalatest.utils.math

import org.scalatest.Suites

class UtilsMathSuite extends Suites(
  new StochasticSpec,
  new ConvolutionSpec,
  new MatrixSpec
)
