package org.openmole.spatialdata.network.real

import org.openmole.spatialdata.network.{Network, NetworkGenerator}

import scala.util.Random

case class MatsimNetworkGenerator(
                                 networkFile: String
                                 ) extends NetworkGenerator {
  override def generateNetwork(implicit rng: Random): Network = MatsimNetworkGenerator.readFromMatsimXML(networkFile)
}



object MatsimNetworkGenerator {

  def readFromMatsimXML(file: String): Network = {
    Network.empty
  }

}
