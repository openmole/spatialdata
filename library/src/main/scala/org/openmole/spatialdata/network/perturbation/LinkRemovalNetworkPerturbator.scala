package org.openmole.spatialdata.network.perturbation

import org.openmole.spatialdata.network._
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random

case class LinkRemovalNetworkPerturbator(
                                        removedLinksProportion: Double
                                        ) extends NetworkPerturbator {

  override def perturbateNetwork(network: Network)(implicit rng: Random): Network =
    removeLinks(network,Stochastic.sampleWithoutReplacement(network.links,math.floor(removedLinksProportion*network.links.size).toInt).toSet,keepNodes = false)

}

