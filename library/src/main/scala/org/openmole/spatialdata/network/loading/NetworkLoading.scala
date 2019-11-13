package org.openmole.spatialdata.network.loading

import org.openmole.spatialdata.network.{Link, Network, Node}


case class NetworkLoading(
                           /**
                             * the loaded network with effective updated costs
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

                         ) {

  def updateFlows(newflows: Map[Link,Double]): NetworkLoading = NetworkLoading.updateFlows(this,newflows)

  def updateCosts(linkCostFunction: Link => Double): NetworkLoading = NetworkLoading.updateCosts(this, linkCostFunction)

}



object NetworkLoading {

  def updateFlows(loading: NetworkLoading, newflows: Map[Link,Double]): NetworkLoading = NetworkLoading(
    loading.loadedNetwork.updateLinkCosts(newflows.keys.toSeq),
    newflows,
    loading.odPattern
  )

  def updateCosts(loading: NetworkLoading, linkCostFunction: Link => Double): NetworkLoading = {
    val newLinks = loading.flows.map{case (l,f) => l.copy(weight = linkCostFunction(f))}
    loading.copy(loadedNetwork = loading.loadedNetwork.updateLinkCosts(newLinks.toSeq))
  }


}