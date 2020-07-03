package org.openmole.spatialdata.network.real

import java.io.FileReader

import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamReader}
import org.openmole.spatialdata.network.{Link, Network, NetworkGenerator, Node}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Read a network from a Matsim xml file
  * @param networkFile file
  */
case class MatsimNetworkGenerator(
                                 networkFile: String
                                 ) extends NetworkGenerator {
  override def generateNetwork(implicit rng: Random): Network = MatsimNetworkGenerator.readFromMatsimXML(networkFile)
}



object MatsimNetworkGenerator {

  def readFromMatsimXML(file: String): Network = {
    val xml: XMLStreamReader = XMLInputFactory.newInstance.createXMLStreamReader(new FileReader(file))

    def parseNextElement(state: (XMLStreamReader,mutable.HashMap[String,Node],ArrayBuffer[Link],Int)): (XMLStreamReader,mutable.HashMap[String,Node],ArrayBuffer[Link],Int) = {
      val reader = state._1
      val eventType = reader.next
      if (eventType==XMLStreamConstants.START_ELEMENT) {
        reader.getLocalName match {
          case "node" =>
            val n: Node = Node(
              //reader.getAttributeValue(null, "id").toLong.toInt,
              state._2.size,
              reader.getAttributeValue(null, "x").toDouble,
              reader.getAttributeValue(null, "y").toDouble
            )
            state._2.put(reader.getAttributeValue(null, "id"),n)
          case "link" =>
            val n1 = state._2.getOrElse(reader.getAttributeValue(null, "from"),Node.empty)
            val n2 = state._2.getOrElse(reader.getAttributeValue(null, "to"),Node.empty)
            state._3.appendAll(Seq(Link(n1,n2)))
          case _ =>
        }
      }
      (reader,state._2,state._3,eventType)
    }
    val nw = Iterator.iterate((xml,mutable.HashMap.empty[String,Node],ArrayBuffer.empty[Link],0.toInt))(parseNextElement).takeWhile(_._4!=XMLStreamConstants.END_DOCUMENT).toSeq.last
    Network(nw._2.values.toSet,nw._3.toSet)
  }

}
