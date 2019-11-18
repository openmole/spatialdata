package org.openmole.spatialdata.network

import scala.util.Random

trait NetworkPerturbator {

  def perturbateNetwork(network: Network)(implicit rng: Random): Network

}
