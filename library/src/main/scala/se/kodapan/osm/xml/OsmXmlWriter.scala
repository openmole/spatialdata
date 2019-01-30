package se.kodapan.osm.xml

import org.apache.commons.lang3.StringEscapeUtils
import se.kodapan.osm.domain._
import se.kodapan.osm.domain.root.PojoRoot
import java.io.IOException
import java.io.Writer
import java.text.DecimalFormat
import java.text.SimpleDateFormat
import java.util.Date
import java.util


/**
  * @author kalle
  * @since 2013-09-02 5:03 PM
  */
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
    if (upload != null) {
      xml.write("' upload='")
      xml.write(if (upload) "true"
      else "false")
      xml.write("'")
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
    import scala.collection.JavaConversions._
    for (node <- root.getNodes.values) {
      write(node)
    }
    import scala.collection.JavaConversions._
    for (way <- root.getWays.values) {
      write(way)
    }
    import scala.collection.JavaConversions._
    for (relation <- root.getRelations.values) {
      write(relation)
    }
  }

  @throws[IOException]
  def writeTags(osmObject: OsmObject): Unit = { // <tag k='landuse' v='farmland' />
    if (osmObject.getTags != null) {
      import scala.collection.JavaConversions._
      for (tag <- osmObject.getTags.entrySet) {
        xml.write("\t\t<tag k='")
        xml.write(tag.getKey)
        xml.write("' v='")
        xml.write(StringEscapeUtils.escapeXml(tag.getValue))
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
    import scala.collection.JavaConversions._
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
      import scala.collection.JavaConversions._
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
    if (osmObject.getId != null) {
      xml.write(" id='")
      xml.write(String.valueOf(osmObject.getId))
      xml.write("'")
    }
    if (osmObject.getTimestamp != null) {
      xml.write(" timestamp='")
      xml.write(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(new Date(osmObject.getTimestamp)))
      xml.write("'")
    }
    if (osmObject.getUid != null) {
      xml.write(" uid='")
      xml.write(String.valueOf(osmObject.getUid))
      xml.write("'")
    }
    if (osmObject.getUser != null) {
      xml.write(" user='")
      xml.write(String.valueOf(osmObject.getUser))
      xml.write("'")
    }
    if (osmObject.getVersion != null) {
      xml.write(" version='")
      xml.write(String.valueOf(osmObject.getVersion))
      xml.write("'")
    }
    if (osmObject.getChangeset != null) {
      xml.write(" changeset='")
      xml.write(String.valueOf(osmObject.getChangeset))
      xml.write("'")
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