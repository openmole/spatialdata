package org.openmole.spatialdata.network.perturbation

import org.openmole.spatialdata.network.{Link, Network, NetworkPerturbator}

import scala.util.Random

case class LinkAdditionNetworkPerturbator(
                                          newLinksGenerationFunction: Network => Set[Link]
                                         ) extends NetworkPerturbator {

  override def perturbateNetwork(network: Network)(implicit rng: Random): Network =
    Network(network = network, additionalLinks = newLinksGenerationFunction(network))

}
