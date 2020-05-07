package org.openmole.spatialdata.scalatest.network

import org.openmole.spatialdata.scalatest.network.synthetic.NetworkSyntheticSuite
import org.scalatest.Suites

class NetworkSuite extends Suites(
  new NetworkSpec,
  new NetworkSyntheticSuite
)

