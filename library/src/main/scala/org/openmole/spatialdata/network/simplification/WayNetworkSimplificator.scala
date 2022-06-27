package org.openmole.spatialdata.network.simplification

import org.openmole.spatialdata.network.{Link, Network, NetworkSimplificator}

case class WayNetworkSimplificator(
                                    combineLength: (Link,Link)=>Double,
                                    combineWeights: (Link,Link)=>Double
                                  ) extends NetworkSimplificator {

  override def simplifyNetwork(network: Network): Network = SimplificationAlgorithm.simplifyNetwork(network, combineLength, combineWeights)

}
