package org.openmole.spatialdata.utils.osm.xml

import java.io._

import org.apache.commons.io.input.ReaderInputStream
import org.openmole.spatialdata.utils.osm._

import scala.collection.mutable

object State extends Enumeration {
  type State = Value
  val none, create, modify, delete = Value
}
/**
  * OSM data parser
  */
object InstantiatedOsmXmlParser {


  class HashConsing[T] extends Serializable {
    private val map = new mutable.HashMap[T, T]()

    def intern(obj: T): T = {
      map.get(obj) match {
        case None => {map.put(obj, obj);obj}
        case Some(t) => t
      }
    }
  }



  var factoryClass:Class[InstantiatedOsmXmlParser] = null

  /**
    * @return a new instance depending on underlying OS. E.g. Android or Java.
    */
  def newInstance = {
    classOf[InstantiatedOsmXmlParser].synchronized {
      if (factoryClass == null) try
        factoryClass = Class.forName(classOf[InstantiatedOsmXmlParser].getName + "Impl").asInstanceOf[Class[InstantiatedOsmXmlParser]]
      catch {
        case e: ClassNotFoundException =>
          throw new RuntimeException(e)
      }
    }

    try
      factoryClass.newInstance
    catch {
      case e: InstantiationException =>
        throw new RuntimeException(e)
      case e: IllegalAccessException =>
        throw new RuntimeException(e)
    }
  }

  object State extends Enumeration {
    type State = Value
    val none, create, modify, delete = Value
  }

}

abstract class InstantiatedOsmXmlParser {
  /**
    * if false, OSM objects with a version greater than +1 of the current object in root will throw an exception.
    */
  protected var allowingMissingVersions = true
  protected var timestampFormat = new OsmXmlTimestampFormat
  protected var root: Root = new PojoRoot
  protected var tagKeyIntern = new InstantiatedOsmXmlParser.HashConsing[String]
  protected var tagValueIntern = new InstantiatedOsmXmlParser.HashConsing[String]
  protected var userIntern = new InstantiatedOsmXmlParser.HashConsing[String]
  protected var roleIntern = new InstantiatedOsmXmlParser.HashConsing[String]

  /**
    * Overrides use to java Reader
    *
    * @param xml
    * @return
    * @throws OsmXmlParserException
    */
  @throws[OsmXmlParserException]
  final def parse(xml: String):InstantiatedOsmXmlParserDelta = parse(new StringReader(xml))

  @throws[OsmXmlParserException]
  def parse(xml: InputStream):InstantiatedOsmXmlParserDelta = try
    parse(new InputStreamReader(xml, "utf8"))
  catch {
    case e: UnsupportedEncodingException =>
      throw new OsmXmlParserException(e)
  }

  /**
    * Overrides use to java InputStream
    *
    * @param xml
    * @return
    * @throws OsmXmlParserException
    */
  @throws[OsmXmlParserException]
  def parse(xml: Reader):InstantiatedOsmXmlParserDelta = parse(new ReaderInputStream(xml, "utf8"))

  def processParsedNode(node: Node, state: State.Value) = {
  }

  def processParsedWay(way: Way, state: State.Value) = {
  }

  def processParsedRelation(relation: Relation, state: State.Value) = {
  }

  def isAllowingMissingVersions = allowingMissingVersions

  def setAllowingMissingVersions(allowingMissingVersions: Boolean) = {
    this.allowingMissingVersions = allowingMissingVersions
  }

  def getTimestampFormat = timestampFormat

  def setTimestampFormat(timestampFormat: Nothing) = {
    this.timestampFormat = timestampFormat
  }

  def getRoot = root

  def setRoot(root: Root) = {
    this.root = root
  }

  def getTagKeyIntern = tagKeyIntern

  def setTagKeyIntern(tagKeyIntern: Nothing) = {
    this.tagKeyIntern = tagKeyIntern
  }

  def getTagValueIntern = tagValueIntern

  def setTagValueIntern(tagValueIntern: Nothing) = {
    this.tagValueIntern = tagValueIntern
  }

  def getUserIntern = userIntern

  def setUserIntern(userIntern: Nothing) = {
    this.userIntern = userIntern
  }

  def getRoleIntern = roleIntern

  def setRoleIntern(roleIntern: Nothing) = {
    this.roleIntern = roleIntern
  }
}


