package org.openmole.spatialdata.utils.osm

import java.io._
import java.text.{DateFormat, FieldPosition, ParsePosition, SimpleDateFormat}
import java.util.Date

import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamException, XMLStreamReader}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.osm.OSMXmlParser._
import org.openmole.spatialdata.utils.osm.OSMObject._

import scala.collection.mutable
import scala.util.control.Breaks._


case class OSMXmlParser(
                         root: OSMRoot,
                         allowingMissingVersions: Boolean = true,
                         timestampFormat: OsmXmlTimestampFormat = new OsmXmlTimestampFormat,
                         tagKeyIntern: HashConsing[String] = new HashConsing[String],
                         tagValueIntern: HashConsing[String] = new HashConsing[String],
                         userIntern: HashConsing[String] = new HashConsing[String],
                         roleIntern: HashConsing[String] = new HashConsing[String]
                       ) {

  /**
    *
    * @param xml xml string
    * @return
    */
  @throws[OsmXmlParserException]
  final def parse(xml: String): OsmXmlParserDelta = parse(new StringReader(xml))

  @throws[OsmXmlParserException]
  def parse(xml: InputStream): OsmXmlParserDelta = try parse(new InputStreamReader(xml, "utf8"))
  catch {
    case e: UnsupportedEncodingException =>
      throw OsmXmlParserException("", e)
  }

  def processParsedNode(node: Node, state: State.Value): Unit = {val _ = (node, state)}

  def processParsedWay(way: Way, state: State.Value): Unit = {val _ = (way, state)}

  def processParsedRelation(relation: Relation, state: State.Value): Unit = {val _ = (relation, state)}


  @throws[StreamException]
  def readerFactory(xml: InputStream):  OSMXmlParser.Stream  = try
    readerFactory(new InputStreamReader(xml, "utf8"))
  catch {
    case e: UnsupportedEncodingException =>
      throw StreamException("",e)
  }

  @throws[OsmXmlParserException]
  def parse(xml: Reader): OsmXmlParserDelta = {
    val started = System.currentTimeMillis
    val sb = new mutable.StringBuilder()
    val delta = OsmXmlParserDelta()
    try {
      val xmlr:  OSMXmlParser.Stream  = readerFactory(xml)
      var current: OSMObject = null
      var currentNode: Node = null
      var currentRelation: Relation = null
      var currentWay: Way = null
      var skipCurrentObject = false
      var state: State.Value = State.none
      var eventType: Int = xmlr.next // START_DOCUMENT
      while (!xmlr.isEndDocument(eventType)) {
        breakable {
          if (xmlr.isStartElement(eventType)) {
            if ("create" == xmlr.getLocalName) state = State.create
            else if ("modify" == xmlr.getLocalName) state = State.modify
            else if ("delete" == xmlr.getLocalName) state = State.delete
            else if ("node" == xmlr.getLocalName) {
              val identity = xmlr.getAttributeValue(null, "id").toLong
              if ((state == State.none) || (state == State.create)) {
                currentNode = root.getNode(identity)
                if (currentNode != null && currentNode.isLoaded && currentNode.getVersion() != null) {
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version <= currentNode.getVersion()) {
                    skipCurrentObject = true
                    break()
                  }
                  else throw OsmXmlParserException("Inconsistency, node " + identity + " already exists.")
                }
                if (currentNode == null) currentNode = new Node(identity)
                currentNode.setLatitude(xmlr.getAttributeValue(null, "lat").toDouble)
                currentNode.setLongitude(xmlr.getAttributeValue(null, "lon").toDouble)
                parseObjectAttributes(xmlr, currentNode, "id", "lat", "lon")
                currentNode.setLoaded(true)
                current = currentNode
                delta.createdNodes.add(currentNode)
                root.add(currentNode)
              }
              else if (state == State.modify) {
                currentNode = root.getNode(identity)
                if (currentNode == null) throw OsmXmlParserException("Inconsistency, node " + identity + " does not exists.")
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version <= currentNode.getVersion()) {
                  utils.log("Inconsistency, old version detected during modify node.")
                  skipCurrentObject = true
                  break()
                }
                else if (version > currentNode.getVersion() + 1 && !allowingMissingVersions) throw OsmXmlParserException("Inconsistency, version " + version + " too great to modify node " + currentNode.id + " with version " + currentNode.getVersion())
                else if (version == currentNode.getVersion()) throw OsmXmlParserException("Inconsistency, found same version in new data during modify node.")
                currentNode.setTags(null)
                currentNode.setAttributes(null)
                currentNode.setLatitude(xmlr.getAttributeValue(null, "lat").toDouble)
                currentNode.setLongitude(xmlr.getAttributeValue(null, "lon").toDouble)
                parseObjectAttributes(xmlr, currentNode, "id", "lat", "lon")
                current = currentNode
                delta.modifiedNodes.add(currentNode)
                root.add(currentNode)
              }
              else if (state == State.delete) {
                val nodeToRemove = root.getNode(identity)
                if (nodeToRemove == null) {
                  utils.log("Inconsistency, node " + identity + " does not exists.")
                  skipCurrentObject = true
                  break()
                }
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version < nodeToRemove.getVersion()) {
                  utils.log("Inconsistency, old version detected during delete node.")
                  skipCurrentObject = true
                  break()
                }
                else if (version > nodeToRemove.getVersion() + 1 && !allowingMissingVersions) throw OsmXmlParserException("Inconsistency, too great version found during delete node.")
                root.remove(nodeToRemove)
                delta.deletedNodes.add(nodeToRemove)
              }
            }
            else if ("way" == xmlr.getLocalName) {
              val identity = xmlr.getAttributeValue(null, "id").toLong
              if ((state == State.none) || (state == State.create)) {
                currentWay = root.getWay(identity)
                if (currentWay != null && currentWay.isLoaded && currentWay.getVersion() != null) {
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version <= currentWay.getVersion()) {
                    utils.log("Inconsistency, old version detected during create way.")
                    skipCurrentObject = true
                    break()
                  }
                  else throw OsmXmlParserException("Inconsistency, way " + identity + " already exists.")
                }
                if (currentWay == null) currentWay = new Way(identity)

                parseObjectAttributes(xmlr, currentWay, "id")
                currentWay.setLoaded(true)
                current = currentWay
                delta.createdWays.add(currentWay)
                root.add(currentWay)// added line
              }
              else if (state == State.modify) {
                currentWay = root.getWay(identity)
                if (currentWay == null) throw OsmXmlParserException("Inconsistency, way " + identity + " does not exists.")
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version <= currentWay.getVersion()) {
                  utils.log("Inconsistency, old version detected during modify way.")
                  skipCurrentObject = true
                  break()
                }
                else if (version > currentWay.getVersion() + 1 && !allowingMissingVersions) throw OsmXmlParserException("Inconsistency, found too great version in new data during modify way.")
                else if (version == currentWay.getVersion()) throw OsmXmlParserException("Inconsistency, found same version in new data during modify way.")
                currentWay.setAttributes(null)
                if (currentWay.nodes != null) {
                  for (node <- currentWay.nodes) {
                    node.getWaysMemberships.remove(node.getWaysMemberships.indexOf(currentWay))
                    root.add(node)
                  }
                }
                currentWay.nodes = new mutable.ArrayBuffer[Node]
                parseObjectAttributes(xmlr, currentWay, "id")
                current = currentWay
                delta.modifiedWays.add(currentWay)
              }
              else if (state == State.delete) {
                val wayToRemove = root.getWay(identity)
                if (wayToRemove == null) {
                  utils.log("Inconsistency, way \" + identity + \" does not exists.")
                  skipCurrentObject = true
                  break()
                }
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version < wayToRemove.getVersion()) {
                  utils.log("Inconsistency, old version detected during delete way.")
                  skipCurrentObject = true
                  break()
                }
                else if (version > wayToRemove.getVersion() + 1 && !allowingMissingVersions) throw OsmXmlParserException("Inconsistency, too great way version found during delete way.")
                root.remove(wayToRemove)
                delta.deletedWays.add(wayToRemove)
              }
            }
            else if ("nd" == xmlr.getLocalName) { // a node reference inside of a way
              if (skipCurrentObject) break()
              val identity = xmlr.getAttributeValue(null, "ref").toLong
              if ((state == State.none) || (state == State.create) || (state == State.modify)) {
                var node = root.getNode(identity)
                if (node == null) {
                  node = new Node(identity)
                  root.add(node)
                }
                node.addWayMembership(currentWay)
                currentWay.addNode(node)
              }
              else if (state == State.delete) {
                //throw new OsmXmlParserException("Lexical error, delete way should not contain <nd> elements.");
              }
            }
            else if ("relation" == xmlr.getLocalName) {
              // multi polygon, etc
              val identity = xmlr.getAttributeValue(null, "id").toLong
              if ((state == State.none) || (state == State.create)) {
                currentRelation = root.getRelation(identity)
                if (currentRelation != null && currentRelation.isLoaded && currentRelation.getVersion() != null) {
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version <= currentRelation.getVersion()) {
                    utils.log("Inconsistency, old version detected during create relation.")
                    skipCurrentObject = true
                    break()
                  }
                  else throw OsmXmlParserException("Inconsistency, relation " + identity + " already exists.")
                }
                if (currentRelation == null) {
                  currentRelation = new Relation(identity)
                }
                parseObjectAttributes(xmlr, currentRelation, "id")
                currentRelation.setLoaded(true)
                current = currentRelation
                delta.createdRelations.add(currentRelation)
              }
              else if (state == State.modify) {
                currentRelation = root.getRelation(identity)
                if (currentRelation == null) throw OsmXmlParserException("Inconsistency, relation " + identity + " does not exists.")
                val version = xmlr.getAttributeValue(null, "version").toInt
                if (version < currentRelation.getVersion()) {
                  utils.log("Inconsistency, old version detected during modify relation.")
                  skipCurrentObject = true
                  break()
                }
                else if (version > currentRelation.getVersion() + 1 && !allowingMissingVersions) throw OsmXmlParserException("Inconsistency, too great version found during modify relation.")
                else if (version == currentRelation.getVersion()) throw OsmXmlParserException("Inconsistency, same version found during modify relation.")
                if (currentRelation.members != null) {

                  for (member <- currentRelation.members) {
                    member.getOsmObject().getRelationMemberships().remove(member.getOsmObject().getRelationMemberships().indexOf(member))
                    if (member.getOsmObject().getRelationMemberships().isEmpty) member.getOsmObject().setRelationMemberships(null)
                  }
                  currentRelation.members = new mutable.ArrayBuffer[RelationMembership]
                }
                currentRelation.setAttributes(null)
                currentRelation.setTags(null)
                current = currentRelation
                parseObjectAttributes(xmlr, currentRelation, "id")
                delta.modifiedRelations.add(currentRelation)
              }
              else if (state == State.delete) {
                val relationToRemove = root.getRelation(identity)
                if (relationToRemove == null) {
                  utils.log("Inconsistency, relation \" + identity + \" does not exist.")
                  skipCurrentObject = true
                  break()
                }
                val version = xmlr.getAttributeValue(null, "version").toInt
                if (version < relationToRemove.getVersion()) {
                  utils.log("Inconsistency, old version detected during delete relation.")
                  skipCurrentObject = true
                  break()
                }
                else if (version > relationToRemove.getVersion() + 1 && !allowingMissingVersions) throw OsmXmlParserException("Inconsistency, too great version found during delete relation.")
                if (relationToRemove.members != null) {
                  for (member <- relationToRemove.members) {
                    member.getOsmObject().getRelationMemberships().remove(member.getOsmObject().getRelationMemberships().indexOf(member))
                    if (member.getOsmObject().getRelationMemberships().isEmpty) member.getOsmObject().setRelationMemberships(null)
                  }
                  relationToRemove.members = new mutable.ArrayBuffer[RelationMembership]
                }
                root.remove(relationToRemove)
                delta.deletedRelations.add(relationToRemove)
              }
            }
            else if ("member" == xmlr.getLocalName) { // multi polygon member
              if (skipCurrentObject) break()
              if ((state == State.none) || (state == State.create) || (state == State.modify)) {
                val member = new RelationMembership
                member.setRelation(currentRelation)
                member.setRole(roleIntern.intern(xmlr.getAttributeValue(null, "role")))
                val identity = xmlr.getAttributeValue(null, "ref").toLong
                val `type` = xmlr.getAttributeValue(null, "type")
                if ("way" == `type`) {
                  var way = root.getWay(identity)
                  if (way == null) {
                    way = new Way(identity)
                    root.add(way)
                  }
                  member.setOsmObject(way)
                }
                else if ("node" == `type`) {
                  var node = root.getNode(identity)
                  if (node == null) {
                    node = new Node(identity)
                    root.add(node)
                  }
                  member.setOsmObject(node)
                }
                else if ("relation" == `type`) {
                  var relation = root.getRelation(identity)
                  if (relation == null) {
                    relation = new Relation(identity)
                    root.add(relation)
                  }
                  member.setOsmObject(relation)
                }
                else throw new RuntimeException("Unsupported relation member type: " + `type`)
                member.getOsmObject().addRelationMembership(member)
                currentRelation.addMember(member)
              }
              else if (state == State.delete) {
                //throw new OsmXmlParserException("Lexical error, delete relation should not contain <member> elements.");
              }
            }
            else if ("tag" == xmlr.getLocalName){ // tag of any object type
              if (skipCurrentObject) break()
              if ((state == State.none) || (state == State.create) || (state == State.modify)) {
                val key = tagKeyIntern.intern(xmlr.getAttributeValue(null, "k"))
                val value = tagValueIntern.intern(xmlr.getAttributeValue(null, "v"))
                current.setTag(key, value)
              }
              else if (state == State.delete) {
                //throw new OsmXmlParserException("Lexical error, delete object should not contain <tag> elements.");
              }
            }
            else if (xmlr.isEndElement(eventType)) if ("create" == xmlr.getLocalName) state = State.none
            else if ("modify" == xmlr.getLocalName) state = State.none
            else if ("delete" == xmlr.getLocalName) state = State.none
            else if ("node" == xmlr.getLocalName) {
              if ((state == State.none) || (state == State.create) || (state == State.modify)) root.add(currentNode)
              processParsedNode(currentNode, state)
              currentNode = null
              current = null
              skipCurrentObject = false
            }
            else if ("way" == xmlr.getLocalName) {
              if ((state == State.none) || (state == State.create) || (state == State.modify)) root.add(currentWay)
              processParsedWay(currentWay, state)
              currentWay = null
              current = null
              skipCurrentObject = false
            }
            else if ("relation" == xmlr.getLocalName) {
              if ((state == State.none) || (state == State.create) || (state == State.modify)) root.add(currentRelation)
              processParsedRelation(currentRelation, state)
              currentRelation = null
              current = null
              skipCurrentObject = false
            }
            else {
              // what not
            }
          }
        }
        sb.append(xmlr.toString)
        eventType = xmlr.next
      }
      sb.append(xmlr.toString)
      xmlr.close()
    } catch {
      case ioe: StreamException =>
        throw OsmXmlParserException("", ioe)
    }
    val timespent = System.currentTimeMillis - started
    utils.log(sb.toString())
    utils.log(s"time spent = $timespent ms")
    delta
  }

  @throws[StreamException]
  private def parseObjectAttributes(xmlr: OSMXmlParser.Stream, osmObject: OSMObject, parsedAttributes: String*): Unit = {
    var attributeIndex = 0
    while (attributeIndex < xmlr.getAttributeCount) {
      val key = xmlr.getAttributeLocalName(attributeIndex)
      val value = xmlr.getAttributeValue(attributeIndex)
      key match {
        case "version" => osmObject.setVersion(value.toInt)
        case "changeset" => osmObject.setChangeset(value.toLong)
        case "uid" => osmObject.setUid(value.toLong)
        case "user" => osmObject.setUser(userIntern.intern(value))
        case "visible" => osmObject.setVisible(value.toBoolean)
        case "timestamp" => osmObject.setTimestamp(timestampFormat.parse(value).getTime) // pe: Exception => throw new RuntimeException(pe)
        case key if !parsedAttributes.contains(key) => osmObject.setAttribute(key, value)
        case _ =>
      }
      attributeIndex += 1
    }
  }



  private val xmlif = XMLInputFactory.newInstance

  @throws[StreamException]
  def readerFactory(xml: Reader):  OSMXmlParser.Stream = {
    var xmlr: XMLStreamReader = null
    try
      xmlr = xmlif.createXMLStreamReader(xml)
    catch {
      case e: XMLStreamException =>
        throw StreamException("",e)
    }
    OSMXmlParser.Stream(xmlr)
  }



}



/**
  * OSM data parser
  */
object OSMXmlParser {


  class HashConsing[T] {
    private val map = new mutable.HashMap[T, T]()

    def intern(obj: T): T = {
      map.get(obj) match {
        case None =>
          map.put(obj, obj)
          obj
        case Some(t) => t
      }
    }
  }

  object State extends Enumeration {
    type State = Value
    val none, create, modify, delete = Value
  }

  /**
    * formats for xml timestamp
    */
  class OsmXmlTimestampFormat extends DateFormat {

    private val format1 = "yyyy-MM-dd'T'HH:mm:ss'Z'"
    private val format2 = "yyyy-MM-dd'T'HH:mm:ss"

    private val implementation1 = new SimpleDateFormat(format1)
    private val implementation2 = new SimpleDateFormat(format2)

    override def format(date: Date, stringBuffer: StringBuffer, fieldPosition: FieldPosition): StringBuffer = implementation1.format(date, stringBuffer, fieldPosition)

    override def parse(s: String, parsePosition: ParsePosition): Date = {
      if (s.length - parsePosition.getIndex == format1.length)
        return implementation1.parse(s, parsePosition)
      implementation2.parse(s, parsePosition)
    }
  }

  case class OsmXmlParserDelta(
                            createdNodes: mutable.HashSet[Node] = new mutable.HashSet[Node],
                            modifiedNodes: mutable.HashSet[Node] = new mutable.HashSet[Node],
                            deletedNodes: mutable.HashSet[Node] = new mutable.HashSet[Node],
                            createdWays: mutable.HashSet[Way] = new mutable.HashSet[Way],
                            modifiedWays: mutable.HashSet[Way] = new mutable.HashSet[Way],
                            deletedWays: mutable.HashSet[Way] = new mutable.HashSet[Way],
                            createdRelations: mutable.HashSet[Relation] = new mutable.HashSet[Relation],
                            modifiedRelations: mutable.HashSet[Relation] = new mutable.HashSet[Relation],
                            deletedRelations: mutable.HashSet[Relation] = new mutable.HashSet[Relation]
                              )

  /**
    * parsing exception
    * @param s message
    * @param throwable exception
    */
  case class OsmXmlParserException(s: String = "", throwable: Throwable = null) extends Exception(s, throwable)

  case class StreamException(message: String = "", cause: Throwable = null) extends Exception(message, cause)

  case class Stream(xmlr: XMLStreamReader) {
    @throws[StreamException]
    def getEventType: Int = xmlr.getEventType

    @throws[StreamException]
    def isEndDocument(eventType: Int): Boolean = eventType == XMLStreamConstants.END_DOCUMENT

    @throws[StreamException]
    def next: Int = try xmlr.next
    catch {
      case e: XMLStreamException =>
        throw StreamException("",e)
    }

    @throws[StreamException]
    def isStartElement(eventType: Int): Boolean = eventType == XMLStreamConstants.START_ELEMENT

    @throws[StreamException]
    def isEndElement(eventType: Int): Boolean = eventType == XMLStreamConstants.END_ELEMENT

    @throws[StreamException]
    def getLocalName: String = xmlr.getLocalName

    @throws[StreamException]
    def getAttributeValue(what: String, key: String): String = xmlr.getAttributeValue(what, key)

    @throws[StreamException]
    def getAttributeCount: Int = xmlr.getAttributeCount

    @throws[StreamException]
    def getAttributeValue(index: Int): String = xmlr.getAttributeValue(index)

    @throws[StreamException]
    def getAttributeLocalName(index: Int): String = xmlr.getAttributeLocalName(index)

    @throws[StreamException]
    def close(): Unit = {
      try
        xmlr.close()
      catch {
        case e: XMLStreamException =>
          throw  StreamException("", e)
      }
    }

    override def toString: String = if (isStartElement(getEventType))
      "<"+getLocalName+(0 until getAttributeCount).map(i => getAttributeLocalName(i)+" = "+getAttributeValue(i)).mkString(" ")+"/>\n"
      else ""

  }


}


