package org.openmole.spatialdata.scalatest

import org.openmole.spatialdata.scalatest.grid.GridSuite
import org.openmole.spatialdata.scalatest.model.ModelSuite
import org.openmole.spatialdata.scalatest.network.NetworkSuite
import org.openmole.spatialdata.scalatest.utils.UtilsSuite
import org.scalatest.Suites

class TestSuite extends Suites(
  new GridSuite,
  new ModelSuite,
  new NetworkSuite,
  new UtilsSuite
)
