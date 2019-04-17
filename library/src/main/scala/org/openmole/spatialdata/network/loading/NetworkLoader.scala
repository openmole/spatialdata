
package org.openmole.spatialdata.network.loading

import org.openmole.spatialdata.network.Network


case class NetworkFlows()


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