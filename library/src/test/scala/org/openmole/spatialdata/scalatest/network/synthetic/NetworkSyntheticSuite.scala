package org.openmole.spatialdata.scalatest.network.synthetic

import org.scalatest.Suites

class NetworkSyntheticSuite extends Suites(
  new RandomNetworkGeneratorSpec,
  new TreeMinDistGeneratorSpec
)

