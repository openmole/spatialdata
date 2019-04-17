package org.openmole.spatialdata.network.loading

import org.openmole.spatialdata.network.{Link, Network, Node}


case class NetworkFlows(
                         /**
                           * the loaded network
                           */
                       loadedNetwork: Network,

                         /**
                           * flows on each link
                           */
                       flows: Map[Link,Double],

                         /**
                           * optional o/d matrix to load the network
                           */
                         odPattern: Option[Map[(Node,Node),Double]]

                       )


/**
  * Compute flows in a network
  *
  *  Implementation choices :
  *   - betweenness based
  *   - wardrop
  *   - pressure equilibrium (type slime mould)
  */
trait NetworkLoader {

  def load(network: Network): NetworkFlows = {
    NetworkFlows()
  }

}