package se.kodapan.osm.services.overpass

import org.apache.commons.io.IOUtils
import org.apache.http.{HttpResponse, NameValuePair}
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.message.BasicNameValuePair
import java.io.InputStreamReader
import java.io.StringWriter
import java.util

import se.kodapan.osm.services.HttpService

/**
  * @author kalle
  * @since 2012-12-31 16:32
  */
object Overpass {
//  private val log = LoggerFactory.getLogger(classOf[Overpass])
}

class Overpass extends HttpService {
  private var serverURL = "http://www.overpass-api.de/api/interpreter"

  /**
    * 2013-07-28 Usage policy accept 10 000 requests or 5GB data per day using up to two threads.
    * See http://wiki.openstreetmap.org/wiki/Overpass_API#Introduction
    *
    * @param overpassQuery
    * @param queryDescription
    * @return
    * @throws Exception
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
    val started = System.currentTimeMillis
    val response = getHttpClient.execute(post)
    val buffer = new StringWriter
    IOUtils.copy(new InputStreamReader(response.getEntity.getContent, "utf8"), buffer)
    val ended = System.currentTimeMillis
//    if (Overpass.log.isInfoEnabled) {
//    println("Overpass response for " + (if (queryDescription != null) queryDescription
//      else "un named query") + " was " + buffer.getBuffer.length + " characters and received in " + (ended - started) + " ms.")
//      if (Overpass.log.isDebugEnabled) Overpass.log.debug(buffer.getBuffer.toString)
//    }
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
