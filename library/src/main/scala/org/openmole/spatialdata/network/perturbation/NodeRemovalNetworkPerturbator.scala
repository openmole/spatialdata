package org.openmole.spatialdata.network.perturbation

import org.openmole.spatialdata.network._
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random

case class NodeRemovalNetworkPerturbator(
                                          removedNodesProportion: Double
                                        ) extends NetworkPerturbator {

  override def perturbateNetwork(network: Network)(implicit rng: Random): Network =
    removeNodes(network,Stochastic.sampleWithoutReplacement(network.nodes,math.floor(removedNodesProportion*network.nodes.size).toInt).toSet)
}
