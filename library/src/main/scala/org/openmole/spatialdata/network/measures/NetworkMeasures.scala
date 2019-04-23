package org.openmole.spatialdata.network.measures

import org.openmole.spatialdata.network._

object NetworkMeasures {


  case class CachedNetworkMeasures()


  case class SummaryNetworkMeasures(
                                     gamma: Double
                                   )

  /**
    * directed network density
    * @param network
    * @return
    */
  def gamma(network: Network): Double = network.links.size / (network.nodes.size * (network.nodes.size - 1))

  case class ShortestPathsNetworkMeasures()

  /**
    * compute network measures linked to shortest paths
    *  - closeness centrality
    *  - betweenness centrality
    *  [- accessibility : node variable ?]
    *  - efficiency
    * @param network
    * @return
    */
  def computeShortestPathsNetworkMeasures(network: Network): ShortestPathsNetworkMeasures = ShortestPathsNetworkMeasures()



}
