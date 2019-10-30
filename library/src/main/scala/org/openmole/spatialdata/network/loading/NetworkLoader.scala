package org.openmole.spatialdata.network.loading

import org.openmole.spatialdata.network.{Link, Network, Node}




/**
  * Compute flows in a network
  *
  *  Implementation choices :
  *   - betweenness based
  *   - wardrop
  *   - pressure equilibrium (type slime mould)
  */
trait NetworkLoader {

  def load(odPattern: Option[Map[(Node,Node),Double]]): NetworkLoading

  def defaultLoading(network: Network) = NetworkLoading(network,network.links.map{l => (l,1.0)}.toMap,None)


}