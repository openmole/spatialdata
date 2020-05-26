package org.openmole.spatialdata.utils.osm

import java.io.{InputStreamReader, StringWriter}
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.regex.Pattern

import org.apache.commons.io.IOUtils
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.client.CredentialsProvider
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{HttpGet, HttpPut}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.{BasicCredentialsProvider, CloseableHttpClient, HttpClientBuilder}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.osm.OSMObject._
import org.openmole.spatialdata.utils.osm.OSMXmlParser.OsmXmlParserDelta

/**
  * http://wiki.openstreetmap.org/wiki/API_v0.6
  */
object APIConnection {
  private val defaultServerURL = "https://api.openstreetmap.org/api"
  private val displayNamePattern = Pattern.compile("<user\\s*.* display_name=\"([^\"]+)\"")
  private val uidPattern = Pattern.compile("<user\\s*.* id=\"([^\"]+)\"")
  val apiVersion = "0.6"
}

class APIConnection(userServerURL: String = APIConnection.defaultServerURL, val timeout: Int = 1000) {

  def removeTrailing(s: (String,Boolean)): (String,Boolean) = if(s._1.endsWith("/")) removeTrailing((s._1.substring(0, s._1.length - 1),false)) else (s._1+"/",true)

  val serverURL: String = Iterator.iterate((userServerURL,false.asInstanceOf[Boolean]))(removeTrailing).takeWhile(!_._2).toSeq.last._1

  private val prefix: String = this.serverURL + "/" + APIConnection.apiVersion + "/"

  val requestBuilder: RequestConfig.Builder = RequestConfig.custom().setConnectTimeout(timeout).setConnectionRequestTimeout(timeout)

  private var httpClient: CloseableHttpClient = HttpClientBuilder.create.setDefaultRequestConfig(requestBuilder.build()).build
//  private var serverURL = null

  //private var httpClient: CloseableHttpClient = _
  private var username: String = ""
  private var uid = 0L
  private var displayName: String = ""
  private val getApiType = new OSMObjectVisitor[String]() {
    override def visit(node: Node) = "node"
    override def visit(way: Way) = "way"
    override def visit(relation: Relation) = "relation"
  }

  @throws[Exception]
  def close(): Unit = httpClient.close()

  @throws[Exception]
  def authenticate(username: String, password: String): Unit = {
    httpClient.close()
    val credsProvider: CredentialsProvider = new BasicCredentialsProvider
    credsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password))
    httpClient = HttpClientBuilder.create.setDefaultCredentialsProvider(credsProvider).setUserAgent("osm-common").build
    this.username = username
    val response = httpClient.execute(new HttpGet(prefix + "user/details"))
    try {
      if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
      var xml = IOUtils.toString(response.getEntity.getContent, "UTF8")
      xml = xml.replaceAll("\n+", " ")
      var matcher = APIConnection.uidPattern.matcher(xml)
      if (!matcher.find) throw new RuntimeException("No user id in xml!\n" + xml)
      this.uid = matcher.group(1).toLong
      matcher = APIConnection.displayNamePattern.matcher(xml)
      if (!matcher.find) throw new RuntimeException("No display name in xml!\n" + xml)
      this.displayName = matcher.group(1)
    } finally response.close()
  }

  @throws[Exception]
  def get(south: Double, west: Double, north: Double, east: Double): OSMRoot = {
    val root = new OSMRoot
    val _ = get(root, south, west, north, east)
    root
  }

  /**
    * HTTP status code 400 (Bad Request)
    * When any of the node/way/relation limits are crossed
    * <p/>
    * HTTP status code 509 (Bandwidth Limit Exceeded)
    * "Error: You have downloaded too much data. Please try again later." See Developer FAQ.
    *
    * @param root root
    * @param south south
    * @param west west
    * @param north north
    * @param east east
    */
  @throws[Exception]
  def get(root: OSMRoot, south: Double, west: Double, north: Double, east: Double): OsmXmlParserDelta = {
    val otherSymbols = new DecimalFormatSymbols
    otherSymbols.setDecimalSeparator('.')
    val df = new DecimalFormat("#.#######", otherSymbols)
    val bottom = df.format(south)
    val left = df.format(west)
    val top = df.format(north)
    val right = df.format(east)
    val response = httpClient.execute(new HttpGet(prefix + "map?bbox=" + left + "," + bottom + "," + right + "," + top))

    utils.log(prefix + "map?bbox=" + left + "," + bottom + "," + right + "," + top)

    try {
      if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
      val parser = OSMXmlParser.apply()
      parser.setRoot(root)
      parser.parse(response.getEntity.getContent)
    } finally response.close()
  }

  /**
    *
    * @param comment comment
    * @return id of new changeset
    */
  @throws[Exception]
  def createChangeset(comment: String): Long = {
    val xml = new StringWriter(1024)
    val generator = getClass.getName + "#createChangeset"
    xml.write("<?xml version='1.0' encoding='UTF-8'?>\n")
    xml.write("<osm version='")
    xml.write(APIConnection.apiVersion)
    xml.write("' generator='")
    xml.write(generator)
    xml.write("'>\n")
    xml.write("<changeset>")
    xml.write("<tag k='created_by' v='")
    xml.write(generator)
    xml.write("'/>")
    if (comment != null) {
      xml.write("<tag k='comment' v='")
      xml.write(comment)
      xml.write("'/>")
    }
    xml.write("</changeset>")
    xml.write("</osm>\n")
    val put = new HttpPut(prefix + "changeset/create")
    put.setEntity(new StringEntity(xml.toString, "UTF8"))

    utils.log(prefix + "changeset/create")
    val response = httpClient.execute(put)
    try {
      if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
      val id = IOUtils.toString(response.getEntity.getContent).toLong
      id
    } finally response.close()
  }

  @throws[Exception]
  def closeChangeset(id: Long): Unit = {
    val put = new HttpGet(prefix + "changeset/" + id + "/close")
    val response = httpClient.execute(put)
    try
        if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
    finally response.close()
  }

  @throws[Exception]
  def getNode(id: Long): Node = {
    val root = new OSMRoot
    val delta = getNode(root, id)
    delta.createdNodes.iterator.next
  }

  @throws[Exception]
  def getNode(root: OSMRoot, id: Long): OsmXmlParserDelta = {
    val get = new HttpGet(prefix + "node/" + id)
    val response = httpClient.execute(get)
    try {
      if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
      val parser = OSMXmlParser.apply()
      parser.setRoot(root)
      parser.parse(new InputStreamReader(response.getEntity.getContent, "UTF8"))
    } finally response.close()
  }

  @throws[Exception]
  def getWay(id: Long): Way = {
    val root = new OSMRoot
    val delta = getWay(root, id)
    delta.createdWays.iterator.next
  }

  @throws[Exception]
  def getWay(root: OSMRoot, id: Long): OsmXmlParserDelta = {
    val get = new HttpGet(prefix + "way/" + id)
    val response = httpClient.execute(get)
    try {
      if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
      val parser = OSMXmlParser.apply()
      parser.setRoot(root)
      parser.parse(new InputStreamReader(response.getEntity.getContent, "UTF8"))
    } finally response.close()
  }

  @throws[Exception]
  def getRelation(id: Long): Relation = {
    val root = new OSMRoot
    val delta = getRelation(root, id)
    delta.createdRelations.iterator.next
  }

  @throws[Exception]
  def getRelation(root: OSMRoot, id: Long): OsmXmlParserDelta = {
    val get = new HttpGet(prefix + "relation/" + id)
    val response = httpClient.execute(get)
    try {
      if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
      val parser = OSMXmlParser.apply()
      parser.setRoot(root)
      parser.parse(new InputStreamReader(response.getEntity.getContent, "UTF8"))
    } finally response.close()
  }

  @throws[Exception]
  def create(changeset: Long, osmObject: OSMObject): Unit = {
    osmObject.setTimestamp(System.currentTimeMillis)
    osmObject.setChangeset(changeset)
    val sw = new StringWriter(4096)
    val xml = new OSMXmlWriter(sw)
    xml.write(osmObject)
    xml.close()
    val put = new HttpPut(prefix + osmObject.accept(getApiType) + "/create")
    put.setEntity(new StringEntity(sw.toString, "UTF8"))
    val response = httpClient.execute(put)
    try {
      if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
      val id = IOUtils.toString(response.getEntity.getContent).toLong
      osmObject.setId(id)
      osmObject.setVersion(1)
      osmObject.setVisible(true)
      osmObject.setUid(uid)
      osmObject.setUser(displayName)
    } finally response.close()
    osmObject.setChangeset(changeset)
  }

  def update(osmObject: OSMObject): Unit = {
    throw new UnsupportedOperationException
  }

  @throws[Exception]
  def delete(changeset: Long, osmObject: OSMObject): Unit = {
    val xml = new StringWriter(1024)
    val generator = getClass.getName + "#createChangeset"
    xml.write("<?xml version='1.0' encoding='UTF-8'?>\n")
    xml.write("<osm version='")
    xml.write(APIConnection.apiVersion)
    xml.write("' generator='")
    xml.write(generator)
    xml.write("'>\n")
    xml.write("<")
    xml.write(osmObject.accept(getApiType))
    xml.write(" id='")
    xml.write(String.valueOf(osmObject.getId))
    xml.write("' version='")
    xml.write(String.valueOf(osmObject.getVersion))
    xml.write("' changeset='")
    xml.write(String.valueOf(changeset))
    osmObject match {
      case node: Node => xml.write("' lat='")
        xml.write(String.valueOf(node.getLatitude))
        xml.write("' lon='")
        xml.write(String.valueOf(node.getLongitude))
      case _ =>
    }
    xml.write("' />")
    xml.write("</osm>\n")
    val put = new HttpPut(prefix + osmObject.accept(getApiType) + "/" + osmObject.getId)
    put.setHeader("X_HTTP_METHOD_OVERRIDE", "DELETE")
    put.setEntity(new StringEntity(xml.toString, "UTF8"))
    val response = httpClient.execute(put)
    try {
      if (response.getStatusLine.getStatusCode != 200) throw new RuntimeException("HTTP status " + response.getStatusLine.getStatusCode + ", " + response.getStatusLine.getReasonPhrase)
      val version = Integer.valueOf(IOUtils.toString(response.getEntity.getContent))
      osmObject.setVersion(version)
      osmObject.setVisible(false)
      osmObject.setUid(uid)
      osmObject.setUser(displayName)
    } finally response.close()
    osmObject.setChangeset(changeset)
  }
}
