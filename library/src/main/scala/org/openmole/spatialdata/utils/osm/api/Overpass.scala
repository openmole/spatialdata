package org.openmole.spatialdata.utils.osm.api

import java.io.{InputStreamReader, StringWriter}
import java.util

import org.apache.commons.io.IOUtils
import org.apache.http.NameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.message.BasicNameValuePair
import org.openmole.spatialdata.utils.http.HttpService
import APIExtractor.Buildings.asPolygonSeq
import org.openmole.spatialdata.utils.osm._
import org.openmole.spatialdata.utils.osm.xml.InstantiatedOsmXmlParser


class Overpass extends HttpService {
  private var serverURL = "http://www.overpass-api.de/api/interpreter"

  /**
    * 2013-07-28 Usage policy accept 10 000 requests or 5GB data per day using up to two threads.
    * See http://wiki.openstreetmap.org/wiki/Overpass_API#Introduction
    *
    * @param overpassQuery
    * @param queryDescription
    * @return
    */
  @throws[OverpassException]
  def execute(overpassQuery: String, queryDescription: String = null) = try {
    val post = new HttpPost(serverURL)
    setUserAgent(post)
    val nameValuePairs = new util.ArrayList[NameValuePair](1)
    nameValuePairs.add(new BasicNameValuePair("data", overpassQuery))
    post.setEntity(new UrlEncodedFormEntity(nameValuePairs))
    leniency
//    println("Executing overpass query: " + queryDescription + "\n" + overpassQuery)
    //val started = System.currentTimeMillis

    var content = getHttpClient.execute(post).getEntity.getContent
    /*
    val res = Try {
      var content = Try{getHttpClient.execute(post).getEntity.getContent}
      while(content.isFailure){
        // FIXME add a max number of tries ?
        TorPoolManager.switchPort(true)
        content = Try {
          getHttpClient.execute(post).getEntity.getContent
        }
        //if(content.isFailure){content.asInstanceOf[Failure].exception.printStackTrace()}
      }

      val buffer = new StringWriter
      IOUtils.copy(new InputStreamReader(content.get, "utf8"), buffer)
      buffer
    }
    */

    //val ended = System.currentTimeMillis
//    if (Overpass.log.isInfoEnabled) {
//    println("Overpass response for " + (if (queryDescription != null) queryDescription
//      else "un named query") + " was " + buffer.getBuffer.length + " characters and received in " + (ended - started) + " ms.")
//      if (Overpass.log.isDebugEnabled) Overpass.log.debug(buffer.getBuffer.toString)
//    }

    val buffer = new StringWriter
    IOUtils.copy(new InputStreamReader(content, "utf8"), buffer)
    buffer.toString
  } catch {
    case e: Exception =>
      throw new OverpassException(e.getMessage, e)
  }

  def getServerURL = serverURL

  def setServerURL(serverURL: String) = {
    this.serverURL = serverURL
  }
}


class OverpassException(message: String, cause: Throwable) extends Exception(message, cause) {}


object TestOverpass extends App {

  import java.io.StringReader

  val overpass = new Overpass
  overpass.setUserAgent("test suite of <https://github.com/karlwettin/osm-common/>");
  overpass.open()

  val root = new PojoRoot
  val parser = InstantiatedOsmXmlParser.newInstance
  parser.setRoot(root)

  parser.parse(new StringReader(overpass.execute(
    """
      |<union>
      |  <bbox-query s="51.249" w="7.148" n="51.251" e="7.152"/>
      |  <recurse type="up"/>
      |</union>
      |<print mode="meta"/>
    """.stripMargin)))
  asPolygonSeq(root.enumerateWays).foreach(println)
}

