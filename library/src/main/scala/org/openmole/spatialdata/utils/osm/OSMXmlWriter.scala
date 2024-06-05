package org.openmole.spatialdata.utils.osm

import java.io.{IOException, Writer}
import java.text.{DecimalFormat, SimpleDateFormat}
import java.util.Date

import org.apache.commons.text.StringEscapeUtils

import org.openmole.spatialdata.utils.osm.OSMObject._

class OSMXmlWriter @throws[IOException]
(var xml: Writer) extends Writer {
  writeHeader()
  private val version = "0.6"
  private val upload = true
  private val generator = getClass.getName

  @throws[IOException]
  def writeHeader(): Unit = {
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
  def writeFooter(): Unit = {
    xml.write("</osm>\n")
  }

  private val writeVisitor = new OSMObjectVisitor[Void]() {
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
  def write(o: OSMObject): Void = {
    o.accept(writeVisitor)
  }

  @throws[IOException]
  def write(root: OSMRoot): Unit = {
    for (node <- root.getNodes) {
      write(node)
    }
    for (way <- root.getWays) {
      write(way)
    }
    for (relation <- root.getRelations) {
      write(relation)
    }
  }

  @throws[IOException]
  def writeTags(osmObject: OSMObject): Unit = { // <tag k='landuse' v='farmland' />
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
    for (node <- way.nodes) {
      xml.append("\t\t<nd ref='")
      xml.append(String.valueOf(node.id))
      xml.append("' />\n")
      node.id
    }
    writeTags(way)
    xml.write("\t</way>\n")
  }

  private val getOsmObjectTypeName = new OSMObjectVisitor[String]() {
    override def visit(node: Node) = "node"

    override

    def visit(way: Way) = "way"

    override

    def visit(relation: Relation) = "relation"
  }

  @throws[IOException]
  def write(relation: Relation): Unit = {
    writeObjectHead(relation)
    xml.write(" >\n")
    if (relation.members != null) {
      for (membership <- relation.members) {
        xml.write("\t\t<member type='")
        xml.write(membership.getOsmObject.accept(getOsmObjectTypeName))
        xml.write("'")
        xml.write(" ref='")
        xml.write(String.valueOf(membership.getOsmObject.id))
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
  private def writeObjectHead(osmObject: OSMObject): Unit = {
    xml.write("\t<")
    xml.append(osmObject.accept(getOsmObjectTypeName))
    Option(osmObject.id) match {
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
      case Some(v) =>
        xml.write(" version='")
        xml.write(String.valueOf(v))
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
  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    if (!wroteHeader) {
      wroteHeader = true
      writeHeader()
    }
    xml.write(cbuf, off, len)
  }

  @throws[IOException]
  override def flush(): Unit = {
    xml.flush()
  }

  @throws[IOException]
  override def close(): Unit = {
    writeFooter()
    xml.close()
  }
}