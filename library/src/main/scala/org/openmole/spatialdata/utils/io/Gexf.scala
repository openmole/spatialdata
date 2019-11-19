
package org.openmole.spatialdata.utils.io

import java.io.{File, FileWriter}

import org.openmole.spatialdata.network.Network
import it.uniroma1.dis.wsngroup.gexf4j.core._
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.viz.PositionImpl
import it.uniroma1.dis.wsngroup.gexf4j.core.impl._

/**
  * Gexf format is useful for temporal networks => dynamical network analysis ?
  *
  * see https://github.com/JusteRaimbault/BiblioData/blob/master/src/main/java/bibliodata/utils/GEXFWriter.java
  *
  */
object Gexf {


  // TODO
  def readGexf(file: String): Network = {
    // the gexf4j has no reader ?
    Network.empty
  }

  /**
    * Write a network to a gexf file
    * @param network
    * @param file
    */
  def writeGexf(network: Network, file: String): Unit = {

    val writer: GexfWriter = new StaxGraphWriter

    val graph = new GexfImpl()

    network.nodes.foreach{n =>
      graph.getGraph.createNode(n.id.toString)
      graph.getGraph.getNode(n.id.toString).setPosition(new PositionImpl(n.x.toFloat,n.y.toFloat,0.0.toFloat))
    }

    network.links.foreach{l =>
      graph.getGraph.getNode(l.e1.id.toString).connectTo(graph.getGraph.getNode(l.e2.id.toString)).setWeight(l.weight.toFloat)
    }

    writer.writeToStream(graph,new FileWriter(new File(file)),"UTF-8")
  }

}

