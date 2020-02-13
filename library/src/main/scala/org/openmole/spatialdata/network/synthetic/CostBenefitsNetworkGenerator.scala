package org.openmole.spatialdata.network.synthetic


import org.openmole.spatialdata.network._

import scala.util.Random


/**
  *
  */
case class CostBenefitsNetworkGenerator(
                                       lambda: Double
                                       ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = Network.empty

}


object CostBenefitsNetworkGenerator {


  // TODO
  def addCostBenefitLinks(network: Network, lambda: Double, generalizedCostFunction: Network => Map[Link,Double]): Network = Network.empty


}
