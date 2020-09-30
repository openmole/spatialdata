package org.openmole.spatialdata.scalatest

import org.openmole.spatialdata.scalatest.grid.GridSuite
import org.openmole.spatialdata.scalatest.model.ModelSuite
import org.openmole.spatialdata.scalatest.network.NetworkSuite
import org.openmole.spatialdata.scalatest.utils.UtilsSuite
import org.openmole.spatialdata.scalatest.vector.VectorSuite
import org.scalatest.{BeforeAndAfterAll, Suites}

class TestSuite extends Suites (
  new GridSuite,
  new ModelSuite,
  new NetworkSuite,
  new UtilsSuite,
  new VectorSuite
) with BeforeAndAfterAll {
  override def beforeAll(): Unit = {
    org.openmole.spatialdata.HEADLESS = true
  }
}
