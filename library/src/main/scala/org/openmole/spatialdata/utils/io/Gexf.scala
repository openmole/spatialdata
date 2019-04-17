
package org.openmole.spatialdata.utils.io

import org.openmole.spatialdata.network.Network

/**
  * Gexf format is useful for temporal networks => dynamical network analysis ?
  */
// TODO see https://github.com/JusteRaimbault/BiblioData/blob/master/src/main/java/bibliodata/utils/GEXFWriter.java
object Gexf {


  // TODO
  def readGexf(file: String): Network = Network.empty

  // TODO
  def writeGexf(network: Network, file: String): Unit = {}

}

