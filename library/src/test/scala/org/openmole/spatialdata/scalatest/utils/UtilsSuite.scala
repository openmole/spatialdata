package org.openmole.spatialdata.scalatest.utils

import org.openmole.spatialdata.scalatest.utils.graph.UtilsGraphSuite
import org.openmole.spatialdata.scalatest.utils.math.UtilsMathSuite
import org.openmole.spatialdata.scalatest.utils.osm.UtilsOSMSuite
import org.scalatest.Suites

class UtilsSuite extends Suites(
  new UtilsGraphSuite,
  new UtilsMathSuite,
  new UtilsOSMSuite
)


