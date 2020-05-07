package org.openmole.spatialdata.scalatest.utils.io

import org.scalatest.Suites

class UtilsIOSuite extends Suites(
  new BinarySpec,
  new CSVSpec
)
