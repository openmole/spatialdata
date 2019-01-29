package se.kodapan.osm.parser.xml.instantiated

import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader
import java.io.Reader

import se.kodapan.osm.parser.xml.instantiated.AbstractStreamingInstantiatedOsmXmlParser.StreamException


/**
  * An .osm.xml and .osc.xml parser
  * into a fully instantiated object graph.
  * <p/>
  * This class is not thread safe!
  *
  * @author kalle
  * @since 2013-03-27 21:41
  */
object InstantiatedOsmXmlParserImpl {
//  private val log = LoggerFactory.getLogger(classOf[InstantiatedOsmXmlParserImpl])
}

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

