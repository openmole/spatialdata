package org.openmole.spatialdata.utils.osm.xml

import java.io.{IOException, Writer}
import java.text.{DecimalFormat, SimpleDateFormat}
import java.util.Date

import org.apache.commons.text.StringEscapeUtils
import org.openmole.spatialdata.utils.osm._


class OsmXmlWriter @throws[IOException]
(var xml: Writer) extends Writer {
  writeHeader()
  private val version = "0.6"
  private val upload = true
  private val generator = getClass.getName

  @throws[IOException]
  def writeHeader() = {
    xml.write("<?xml version='1.0' encoding='UTF-8'?>\n")
    xml.write("<osm version='")
    xml.write(version)
    Option(upload) match {
      case Some(up) =>
        xml.write("' upload='")
        xml.write((if (up) "true" else "false").toString)
        xml.write("'")
      case None =>
    }
    xml.write(" generator='")
    xml.write(generator)
    xml.write("'>\n")
  }

  @throws[IOException]
  def writeFooter() = {
    xml.write("</osm>\n")
  }

  private val writeVisitor = new OsmObjectVisitor[Void]() {
    override def visit(node: Node): Void = {
      try
        write(node)
      catch {
        case ioe: IOException =>
          throw new RuntimeException(ioe)
      }
      null
    }

    override def visit(way: Way): Void = {
      try
        write(way)
      catch {
        case ioe: IOException =>
          throw new RuntimeException(ioe)
      }
      null
    }

    override def visit(relation: Relation): Void = {
      try
        write(relation)
      catch {
        case ioe: IOException =>
          throw new RuntimeException(ioe)
      }
      null
    }
  }

  @throws[IOException]
  def write(`object`: OsmObject): Void = {
    `object`.accept(writeVisitor)
  }

  @throws[IOException]
  def write(root: PojoRoot): Unit = {
    for (node <- root.getNodes.values) {
      write(node)
    }
    for (way <- root.getWays.values) {
      write(way)
    }
    for (relation <- root.getRelations.values) {
      write(relation)
    }
  }

  @throws[IOException]
  def writeTags(osmObject: OsmObject): Unit = { // <tag k='landuse' v='farmland' />
    if (osmObject.getTags != null) {

      for (tag <- osmObject.getTags.toSeq) {
        xml.write("\t\t<tag k='")
        xml.write(tag._1)
        xml.write("' v='")
        xml.write(StringEscapeUtils.escapeXml10(tag._2))
        xml.write("' />\n")
      }
    }
  }

  @throws[IOException]
  def write(node: Node): Unit = {
    writeObjectHead(node)
    xml.write(" lat='")
    xml.write(new DecimalFormat("#.##################################").format(node.getLatitude))
    xml.write("'")
    xml.write(" lon='")
    xml.write(new DecimalFormat("#.##################################").format(node.getLongitude))
    xml.write("'")
    xml.write(" >\n")
    writeTags(node)
    xml.write("\t</node>\n")
  }

  @throws[IOException]
  def write(way: Way): Unit = {
    writeObjectHead(way)
    xml.write(" >\n")
    for (node <- way.getNodes) {
      xml.append("\t\t<nd ref='")
      xml.append(String.valueOf(node.getId))
      xml.append("' />\n")
      node.getId
    }
    writeTags(way)
    xml.write("\t</way>\n")
  }

  private val getOsmObjectTypeName = new OsmObjectVisitor[String]() {
    override def visit(node: Node) = "node"

    override

    def visit(way: Way) = "way"

    override

    def visit(relation: Relation) = "relation"
  }

  @throws[IOException]
  def write(relation: Relation) = { // <relation id='3146471' timestamp='2013-08-16T01:39:33Z' uid='194367' user='Karl Wettin' visible='true' version='1' changeset='17366616'>
    writeObjectHead(relation)
    xml.write(" >\n")
    if (relation.getMembers != null) {
      for (membership <- relation.getMembers) {
        xml.write("\t\t<member type='")
        xml.write(membership.getObject.accept(getOsmObjectTypeName))
        xml.write("'")
        xml.write(" ref='")
        xml.write(String.valueOf(membership.getObject.getId))
        xml.write("'")
        xml.write(" role='")
        xml.write(membership.getRole)
        xml.write("'")
        xml.write(" />\n")
      }
    }
    writeTags(relation)
    xml.write("\t</relation>\n")
  }

  @throws[IOException]
  private def writeObjectHead(osmObject: OsmObject) = {
    xml.write("\t<")
    xml.append(osmObject.accept(getOsmObjectTypeName))
    Option(osmObject.getId) match {
      case Some(id) =>
        xml.write(" id='")
        xml.write(String.valueOf(id))
        xml.write("'")
      case None =>
    }
    Option(osmObject.getTimestamp) match {
      case Some(timestamp) =>
        xml.write(" timestamp='")
        xml.write(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(new Date(timestamp)))
        xml.write("'")
      case None =>
    }
    Option(osmObject.getUid) match {
      case Some(uid) =>
        xml.write(" uid='")
        xml.write(String.valueOf(uid))
        xml.write("'")
      case None =>
    }
    Option(osmObject.getUser) match {
      case Some(user) =>
        xml.write(" user='")
        xml.write(String.valueOf(user))
        xml.write("'")
      case None =>
    }
    Option(osmObject.getVersion) match {
      case Some(version) =>
        xml.write(" version='")
        xml.write(String.valueOf(version))
        xml.write("'")
      case None =>
    }
    Option(osmObject.getChangeset) match {
      case Some(changeSet) =>
        xml.write(" changeset='")
        xml.write(String.valueOf(changeSet))
        xml.write("'")
      case None =>
    }
  }

  private var wroteHeader = false

  @throws[IOException]
  override def write(cbuf: Array[Char], off: Int, len: Int) = {
    if (!wroteHeader) {
      wroteHeader = true
      writeHeader()
    }
    xml.write(cbuf, off, len)
  }

  @throws[IOException]
  override def flush() = {
    xml.flush()
  }

  @throws[IOException]
  override def close() = {
    writeFooter()
    xml.close()
  }
}