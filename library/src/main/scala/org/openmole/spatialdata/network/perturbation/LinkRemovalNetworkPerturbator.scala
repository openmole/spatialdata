package org.openmole.spatialdata.network.perturbation

import org.openmole.spatialdata.network._
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random

/**
  * Remove links
  * @param removedLinksProportion
  * @param removedLinkChoiceFunction
  */
case class LinkRemovalNetworkPerturbator(
                                        removedLinksProportion: Double = 0.0,
                                        removedLinkChoiceFunction: Link => Boolean = _ => false
                                        ) extends NetworkPerturbator {

  override def perturbateNetwork(network: Network)(implicit rng: Random): Network = {
    val removed = if(removedLinksProportion > 0) Stochastic.sampleWithoutReplacement(network.links, math.floor(removedLinksProportion * network.links.size).toInt).toSet
    else network.links.filter(removedLinkChoiceFunction)
    network.removeLinks( removed, keepNodes = false)
  }
}

