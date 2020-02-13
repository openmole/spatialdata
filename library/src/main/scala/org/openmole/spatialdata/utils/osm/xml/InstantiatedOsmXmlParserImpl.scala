package org.openmole.spatialdata.utils.osm.xml

import java.io.Reader

import javax.xml.stream._
import org.openmole.spatialdata.utils.osm.xml.AbstractStreamingInstantiatedOsmXmlParser.StreamException
//{XMLInputFactory, XMLStreamConstants, XMLStreamException, XMLStreamReader}




class InstantiatedOsmXmlParserImpl extends AbstractStreamingInstantiatedOsmXmlParser {
  private val xmlif = XMLInputFactory.newInstance

  @throws[StreamException]
  override def readerFactory(xml: Reader): AbstractStreamingInstantiatedOsmXmlParser.Stream = {
    var xmlr: XMLStreamReader = null
    try
      xmlr = xmlif.createXMLStreamReader(xml)
    catch {
      case e: XMLStreamException =>
        throw new StreamException(e)
    }
    new AbstractStreamingInstantiatedOsmXmlParser.Stream() {
      @throws[StreamException]
      def getEventType: Int = return xmlr.getEventType

      @throws[StreamException]
      def isEndDocument(eventType: Int): Boolean = return eventType == XMLStreamConstants.END_DOCUMENT

      @throws[StreamException]
      def next: Int = try
        return xmlr.next
      catch {
        case e: XMLStreamException =>
          throw new StreamException(e)
      }

      @throws[StreamException]
      def isStartElement(eventType: Int): Boolean = return eventType == XMLStreamConstants.START_ELEMENT

      @throws[StreamException]
      def isEndElement(eventType: Int): Boolean = return eventType == XMLStreamConstants.END_ELEMENT

      @throws[StreamException]
      def getLocalName: String = return xmlr.getLocalName

      @throws[StreamException]
      def getAttributeValue(what: String, key: String): String = return xmlr.getAttributeValue(what, key)

      @throws[StreamException]
      def getAttributeCount: Int = return xmlr.getAttributeCount

      @throws[StreamException]
      def getAttributeValue(index: Int): String = return xmlr.getAttributeValue(index)

      @throws[StreamException]
      def getAttributeLocalName(index: Int): String = return xmlr.getAttributeLocalName(index)

      @throws[StreamException]
      def close() = {
        try
          xmlr.close()
        catch {
          case e: XMLStreamException =>
            throw new StreamException(e)
        }
      }
    }
  }
}

