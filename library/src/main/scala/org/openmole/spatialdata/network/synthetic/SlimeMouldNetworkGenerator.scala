package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.network._

import scala.util.Random


case class SlimeMouldNetworkGenerator(

                                     ) extends NetworkGenerator {
  override def generateNetwork(implicit rng: Random): Network = empty
}


object SlimeMouldNetworkGenerator {




}
