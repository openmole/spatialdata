package org.openmole.spatialdata.utils.osm.xml

import java.io.{InputStream, InputStreamReader, Reader, UnsupportedEncodingException}
import java.text.ParseException

import org.apache.commons.io.input.ReaderInputStream
import org.openmole.spatialdata.utils.osm._
import org.openmole.spatialdata.utils.osm.xml.AbstractStreamingInstantiatedOsmXmlParser.StreamException

import scala.util.control.Breaks


/**
  * OSM parser
  */
object AbstractStreamingInstantiatedOsmXmlParser {
//  private val log = LoggerFactory.getLogger(classOf[AbstractStreamingInstantiatedOsmXmlParser])

  class StreamException(message: String, cause: Throwable) extends Exception(message, cause) {
    def this(message: String) {
      this(message, null)
    }

    def this(cause: Throwable) {
      this("", cause)
    }
  }

  abstract class Stream {
    @throws[StreamException]
    def getEventType: Int

    @throws[StreamException]
    def isEndDocument(eventType: Int): Boolean

    @throws[StreamException]
    def next: Int

    @throws[StreamException]
    def isStartElement(eventType: Int): Boolean

    @throws[StreamException]
    def isEndElement(eventType: Int): Boolean

    @throws[StreamException]
    def getLocalName: String

    @throws[StreamException]
    def getAttributeValue(what: String, key: String): String

    @throws[StreamException]
    def getAttributeCount: Int

    @throws[StreamException]
    def getAttributeValue(index: Int): String

    @throws[StreamException]
    def getAttributeLocalName(index: Int): String

    @throws[StreamException]
    def close(): Unit
  }

}

abstract class AbstractStreamingInstantiatedOsmXmlParser extends InstantiatedOsmXmlParser {
  @throws[StreamException]
  def readerFactory(xml: Reader): AbstractStreamingInstantiatedOsmXmlParser.Stream = readerFactory(new ReaderInputStream(xml, "utf8"))

  @throws[StreamException]
  def readerFactory(xml: InputStream): AbstractStreamingInstantiatedOsmXmlParser.Stream = try
    readerFactory(new InputStreamReader(xml, "utf8"))
  catch {
    case e: UnsupportedEncodingException =>
      throw new AbstractStreamingInstantiatedOsmXmlParser.StreamException(e)
  }

  @throws[OsmXmlParserException]
  override def parse(xml: Reader) = {
    val started = System.currentTimeMillis
//    AbstractStreamingInstantiatedOsmXmlParser.log.debug("Begin parsing...")
//    println("Begin parsing...")
    val delta = new InstantiatedOsmXmlParserDelta
    try {
      val xmlr: AbstractStreamingInstantiatedOsmXmlParser.Stream = readerFactory(xml)
      var current: OsmObject = null
      var currentNode: Node = null
      var currentRelation: Relation = null
      var currentWay: Way = null
      var skipCurrentObject = false
      var state: State.Value = State.none
      var eventType = xmlr.getEventType // START_DOCUMENT
      eventType = xmlr.next
      while (!xmlr.isEndDocument(eventType)) {
        import util.control.Breaks._
        breakable {
          if (xmlr.isStartElement(eventType)) {
            if ("create" == xmlr.getLocalName) state = State.create
            else if ("modify" == xmlr.getLocalName) state = State.modify
            else if ("delete" == xmlr.getLocalName) state = State.delete
            else if ("node" == xmlr.getLocalName) {
              /**
                *
                * NN  NN   OOOO   DDDDD  EEEEE
                * NNN NN  OO  OO  DD  DD EE
                * NNNNNN  OO  OO  DD  DD EEEE
                * NN NNN  OO  OO  DD  DD EE
                * NN  NN   OOOO   DDDDD  EEEEE
                */
              val identity = xmlr.getAttributeValue(null, "id").toLong
              if ((state == State.none) || (state == State.create)) {
                currentNode = root.getNode(identity)
                if (currentNode != null && currentNode.isLoaded && currentNode.getVersion != null) {
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version <= currentNode.getVersion) {
                    //              AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during create node.")
                    skipCurrentObject = true
                    break //was:continue
                    //              } else if (version > currentNode.getVersion() + 1) {
                    //                throw new OsmXmlParserException("Inconsistency, too great version found during create node.");
                  }
                  else throw new OsmXmlParserException("Inconsistency, node " + identity + " already exists.")
                }
                if (currentNode == null) {
                  currentNode = new Node
                  currentNode.setId(identity)
                }
                currentNode.setLatitude(xmlr.getAttributeValue(null, "lat").toDouble)
                currentNode.setLongitude(xmlr.getAttributeValue(null, "lon").toDouble)
                parseObjectAttributes(xmlr, currentNode, "id", "lat", "lon")
                currentNode.setLoaded(true)
                current = currentNode
                delta.getCreatedNodes.add(currentNode)
                root.add(currentNode)
              }
              else if (state == State.modify) {
                currentNode = root.getNode(identity)
                if (currentNode == null) throw new OsmXmlParserException("Inconsistency, node " + identity + " does not exists.")
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version <= currentNode.getVersion) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during modify node.")
                  println("Inconsistency, old version detected during modify node.")
                  skipCurrentObject = true
                  break //was:continue
                }
                else if (version > currentNode.getVersion + 1 && !isAllowingMissingVersions) throw new OsmXmlParserException("Inconsistency, version " + version + " too great to modify node " + currentNode.getId + " with version " + currentNode.getVersion)
                else if (version == currentNode.getVersion) throw new OsmXmlParserException("Inconsistency, found same version in new data during modify node.")
                currentNode.setTags(null)
                currentNode.setAttributes(null)
                currentNode.setLatitude(xmlr.getAttributeValue(null, "lat").toDouble)
                currentNode.setLongitude(xmlr.getAttributeValue(null, "lon").toDouble)
                parseObjectAttributes(xmlr, currentNode, "id", "lat", "lon")
                current = currentNode
                delta.getModifiedNodes.add(currentNode)
                root.add(currentNode)
              }
              else if (state == State.delete) {
                val nodeToRemove = root.getNode(identity)
                if (nodeToRemove == null) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, node " + identity + " does not exists.")
                  println("Inconsistency, node " + identity + " does not exists.")
                  skipCurrentObject = true
                  break //was:continue
                }
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version < nodeToRemove.getVersion) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during delete node.")
                  println("Inconsistency, old version detected during delete node.")
                  skipCurrentObject = true
                  break //was:continue
                }
                else if (version > nodeToRemove.getVersion + 1 && !isAllowingMissingVersions) throw new OsmXmlParserException("Inconsistency, too great version found during delete node.")
                root.remove(nodeToRemove)
                delta.getDeletedNodes.add(nodeToRemove)
              }
            }
            else if ("way" == xmlr.getLocalName) {
              /**
                *
                * WW  WW  WW   AA   YY  YY
                * WW WW WW   AAAA   YYYY
                * WWWWWWWW  AA  AA   YY
                * WW  WW   AAAAAA   YY
                * WW  WW   AA  AA   YY
                */
              val identity = xmlr.getAttributeValue(null, "id").toLong
              if ((state == State.none) || (state == State.create)) {
                currentWay = root.getWay(identity)
                if (currentWay != null && currentWay.isLoaded && currentWay.getVersion != null) {
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version <= currentWay.getVersion) {
                    //              AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during create way.")
                    println("Inconsistency, old version detected during create way.")
                    skipCurrentObject = true
                    break //was:continue
                    //              } else if (version > currentWay.getVersion() + 1) {
                    //                throw new OsmXmlParserException("Inconsistency, too great version found during create way.");
                  }
                  else throw new OsmXmlParserException("Inconsistency, way " + identity + " already exists.")
                }
                if (currentWay == null) {
                  currentWay = new Way
                  currentWay.setId(identity)
                }
                parseObjectAttributes(xmlr, currentWay, "id")
                currentWay.setLoaded(true)
                current = currentWay
                delta.getCreatedWays.add(currentWay)
                root.add(currentWay)//FIXME: added line
              }
              else if (state == State.modify) {
                currentWay = root.getWay(identity)
                if (currentWay == null) throw new OsmXmlParserException("Inconsistency, way " + identity + " does not exists.")
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version <= currentWay.getVersion) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during modify way.")
                  println("Inconsistency, old version detected during modify way.")
                  skipCurrentObject = true
                  break //was:continue
                }
                else if (version > currentWay.getVersion + 1 && !isAllowingMissingVersions) throw new OsmXmlParserException("Inconsistency, found too great version in new data during modify way.")
                else if (version == currentWay.getVersion) throw new OsmXmlParserException("Inconsistency, found same version in new data during modify way.")
//                currentWay.setTags(null)
                currentWay.setAttributes(null)
                if (currentWay.getNodes != null) {
                  for (node <- currentWay.getNodes) {
                    node.getWaysMemberships.remove(node.getWaysMemberships.indexOf(currentWay))
                    root.add(node)
                  }
                }
                currentWay.setNodes(null)
                parseObjectAttributes(xmlr, currentWay, "id")
                current = currentWay
                delta.getModifiedWays.add(currentWay)
              }
              else if (state == State.delete) {
                val wayToRemove = root.getWay(identity)
                if (wayToRemove == null) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, way " + identity + " does not exists.")
                  println("Inconsistency, way \" + identity + \" does not exists.")
                  skipCurrentObject = true
                  break //was:continue
                }
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version < wayToRemove.getVersion) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during delete way.")
                  println("Inconsistency, old version detected during delete way.")
                  skipCurrentObject = true
                  break //was:continue
                }
                else if (version > wayToRemove.getVersion + 1 && !isAllowingMissingVersions) throw new OsmXmlParserException("Inconsistency, too great way version found during delete way.")
                root.remove(wayToRemove)
                delta.getDeletedWays.add(wayToRemove)
              }
            }
            else if ("nd" == xmlr.getLocalName) { // a node reference inside of a way
              if (skipCurrentObject) break //was:continue
              //todo: continue is not supported
              val identity = xmlr.getAttributeValue(null, "ref").toLong
              if ((state == State.none) || (state == State.create) || (state == State.modify)) {
                var node = root.getNode(identity)
                if (node == null) {
                  node = new Node
                  node.setId(identity)
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
              /**
                *
                * RRRRR  EEEEEE  LL       AA   TTTTTTTT  II   OOOOO   NN  NN
                * RR  RR EE      LL      AAAA     TT     II  OO   OO  NNN NN
                * RRRRR  EEEEEE  LL     AA  AA    TT     II  OO   OO  NNNNNN
                * RR  RR EE      LL     AAAAAA    TT     II  OO   OO  NN NNN
                * RR  RR EEEEEE  LLLLL  AA  AA    TT     II   OOOOO   NN  NN
                */
              // multi polygon, etc
              val identity = xmlr.getAttributeValue(null, "id").toLong
              if ((state == State.none) || (state == State.create)) {
                currentRelation = root.getRelation(identity)
                if (currentRelation != null && currentRelation.isLoaded && currentRelation.getVersion != null) {
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version <= currentRelation.getVersion) {
                    //              AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during create relation.")
                    println("Inconsistency, old version detected during create relation.")
                    skipCurrentObject = true
                    break //was:continue
                    //              } else if (version > currentRelation.getVersion() + 1) {
                    //                throw new OsmXmlParserException("Inconsistency, too great version found during create relation.");
                  }
                  else throw new OsmXmlParserException("Inconsistency, relation " + identity + " already exists.")
                }
                if (currentRelation == null) {
                  currentRelation = new Relation
                  currentRelation.setId(identity)
                }
                parseObjectAttributes(xmlr, currentRelation, "id")
                currentRelation.setLoaded(true)
                current = currentRelation
                delta.getCreatedRelations.add(currentRelation)
              }
              else if (state == State.modify) {
                currentRelation = root.getRelation(identity)
                if (currentRelation == null) throw new OsmXmlParserException("Inconsistency, relation " + identity + " does not exists.")
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version < currentRelation.getVersion) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during modify relation.")
                  println("Inconsistency, old version detected during modify relation.")
                  skipCurrentObject = true
                  break //was:continue
                }
                else if (version > currentRelation.getVersion + 1 && !isAllowingMissingVersions) throw new OsmXmlParserException("Inconsistency, too great version found during modify relation.")
                else if (version == currentRelation.getVersion) throw new OsmXmlParserException("Inconsistency, same version found during modify relation.")
                if (currentRelation.getMembers != null) {

                  for (member <- currentRelation.getMembers) {
                    member.getObject.getRelationMemberships.remove(member.getObject.getRelationMemberships.indexOf(member))
                    if (member.getObject.getRelationMemberships.isEmpty) member.getObject.setRelationMemberships(null)
                  }
                  currentRelation.setMembers(null)
                }
                currentRelation.setAttributes(null)
                currentRelation.setTags(null)
                current = currentRelation
                parseObjectAttributes(xmlr, currentRelation, "id")
                delta.getModifiedRelations.add(currentRelation)
              }
              else if (state == State.delete) {
                val relationToRemove = root.getRelation(identity)
                if (relationToRemove == null) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, relation " + identity + " does not exist.")
                  println("Inconsistency, relation \" + identity + \" does not exist.")
                  skipCurrentObject = true
                  break //was:continue
                }
                val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                if (version < relationToRemove.getVersion) {
                  //            AbstractStreamingInstantiatedOsmXmlParser.log.warn("Inconsistency, old version detected during delete relation.")
                  println("Inconsistency, old version detected during delete relation.")
                  skipCurrentObject = true
                  break //was:continue
                }
                else if (version > relationToRemove.getVersion + 1 && !isAllowingMissingVersions) throw new OsmXmlParserException("Inconsistency, too great version found during delete relation.")
                if (relationToRemove.getMembers != null) {
                  for (member <- relationToRemove.getMembers) {
                    member.getObject.getRelationMemberships.remove(member.getObject.getRelationMemberships.indexOf(member))
                    if (member.getObject.getRelationMemberships.isEmpty) member.getObject.setRelationMemberships(null)
                  }
                  relationToRemove.setMembers(null)
                }
                root.remove(relationToRemove)
                delta.getDeletedRelations.add(relationToRemove)
              }
            }
            else if ("member" == xmlr.getLocalName) { // multi polygon member
              if (skipCurrentObject) break //was:continue
              if ((state == State.none) || (state == State.create) || (state == State.modify)) {
                val member = new RelationMembership
                member.setRelation(currentRelation)
                member.setRole(roleIntern.intern(xmlr.getAttributeValue(null, "role")))
                val identity = xmlr.getAttributeValue(null, "ref").toLong
                val `type` = xmlr.getAttributeValue(null, "type")
                if ("way" == `type`) {
                  var way = root.getWay(identity)
                  if (way == null) {
                    way = new Way
                    way.setId(identity)
                    root.add(way)
                  }
                  member.setObject(way)
                }
                else if ("node" == `type`) {
                  var node = root.getNode(identity)
                  if (node == null) {
                    node = new Node
                    node.setId(identity)
                    root.add(node)
                  }
                  member.setObject(node)
                }
                else if ("relation" == `type`) {
                  var relation = root.getRelation(identity)
                  if (relation == null) {
                    relation = new Relation
                    relation.setId(identity)
                    root.add(relation)
                  }
                  member.setObject(relation)
                }
                else throw new RuntimeException("Unsupported relation member type: " + `type`)
                member.getObject.addRelationMembership(member)
                currentRelation.addMember(member)
              }
              else if (state == State.delete) {
                //throw new OsmXmlParserException("Lexical error, delete relation should not contain <member> elements.");
              }
            }
            else if ("tag" == xmlr.getLocalName) { // tag of any object type
              if (skipCurrentObject) break //was:continue
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
        eventType = xmlr.next
      }
      xmlr.close()
    } catch {
      case ioe: AbstractStreamingInstantiatedOsmXmlParser.StreamException =>
        throw new OsmXmlParserException(ioe)
    }
//    AbstractStreamingInstantiatedOsmXmlParser.log.debug("Done parsing.")
//    AbstractStreamingInstantiatedOsmXmlParser.log.debug("Delta " + delta.getCreatedNodes.size + "/" + delta.getModifiedNodes.size + "/" + delta.getDeletedNodes.size + " nodes, " + delta.getCreatedWays.size + "/" + delta.getModifiedWays.size + "/" + delta.getDeletedWays.size + " ways, " + delta.getCreatedRelations.size + "/" + delta.getModifiedRelations.size + "/" + delta.getDeletedRelations.size + " relations created/modified/deleted.")
//    println("Done parsing.")
//    println("Delta " + delta.getCreatedNodes.size + "/" + delta.getModifiedNodes.size + "/" + delta.getDeletedNodes.size + " nodes, " + delta.getCreatedWays.size + "/" + delta.getModifiedWays.size + "/" + delta.getDeletedWays.size + " ways, " + delta.getCreatedRelations.size + "/" + delta.getModifiedRelations.size + "/" + delta.getDeletedRelations.size + " relations created/modified/deleted.")
    val timespent = System.currentTimeMillis - started
//    AbstractStreamingInstantiatedOsmXmlParser.log.info("Parsed in " + timespent + " milliseconds.")
    delta
  }

  @throws[StreamException]
  private def parseObjectAttributes(xmlr: AbstractStreamingInstantiatedOsmXmlParser.Stream, `object`: OsmObject, parsedAttributes: String*) = {
    var attributeIndex = 0
    while ( {
      attributeIndex < xmlr.getAttributeCount
    }) {
      val key = xmlr.getAttributeLocalName(attributeIndex)
      val value = xmlr.getAttributeValue(attributeIndex)
      if ("version" == key) `object`.setVersion(Integer.valueOf(value))
      else if ("changeset" == key) `object`.setChangeset(value.toLong)
      else if ("uid" == key) `object`.setUid(value.toLong)
      else if ("user" == key) `object`.setUser(userIntern.intern(value))
      else if ("visible" == key) `object`.setVisible(value.toBoolean)
      else if ("timestamp" == key) try {
        `object`.setTimestamp(timestampFormat.parse(value).getTime)
      } catch {
        case pe: ParseException =>
          throw new RuntimeException(pe)
      }
      else {
        var parsed = false
        Breaks.breakable {
          for (parsedAttribute <- parsedAttributes) {
            if (parsedAttribute == key) {
              parsed = true
              Breaks.break
            }
          }
        }
        if (!parsed) {
          `object`.setAttribute(key, value)
//          AbstractStreamingInstantiatedOsmXmlParser.log.warn("Unknown attribute " + key + "='" + value + "' added to object")
        }
      }
      {
        attributeIndex += 1; attributeIndex - 1
      }
    }
  }
}
