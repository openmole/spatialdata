
package org.openmole.spatialdata.utils.io

import java.io.{BufferedWriter, FileWriter}

import org.openmole.spatialdata.network.Network

/**
  * Gexf format is useful for temporal networks => dynamical network analysis ?
  *
  * https://gephi.org/gexf/format/data.html
  *
  * see https://github.com/JusteRaimbault/BiblioData/blob/master/src/main/java/bibliodata/utils/GEXFWriter.java
  *
  */
object Gexf {

  val GEXF_HEADER: String =
    """
      |<?xml version="1.0" encoding="UTF-8"?>
      |<gexf xmlns="http://www.gexf.net/1.2draft" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd" version="1.2">
      |   <graph defaultedgetype="undirected">
    """.stripMargin
  // attributes
  //<attributes class=”node”>
  //  <attribute id=”0” title=”url” type=”string”/>
  //</attributes>

  val GEXF_FOOTER: String =
    """
      |    </graph>
      |</gexf>
    """.stripMargin

  def readGexf(file: String): Network = {
    // the gexf4j has no reader ?
    Network.empty
  }

  /**
    * Write a network to a gexf file
    * @param network network
    * @param file file
    */
  def writeGexf(network: Network, file: String): Unit = {

    val xml = new BufferedWriter(new FileWriter(file))

    xml.write(GEXF_HEADER)

    xml.write("<nodes>\n")
    network.nodes.foreach{n =>
      xml.write(s"<node id='${n.id}'/>")
      //(n.x.toFloat,n.y.toFloat,0.0.toFloat) // position
    }
    xml.write("</nodes>\n")

    xml.write("<edges>\n")
    network.links.zipWithIndex.foreach{case (l,i) =>
      xml.write(s"<edge id='$i' source='${l.e1.id}' target='${l.e2.id}' weight='${l.weight.toString}'/>")
    }
    xml.write("</edges>\n")

    xml.write(GEXF_FOOTER)
    xml.close()
  }

}

