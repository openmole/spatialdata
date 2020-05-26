package org.openmole.spatialdata.utils.osm

import java.io.{InputStreamReader, StringReader, StringWriter}
import java.util

import org.apache.commons.io.IOUtils
import org.apache.http.NameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.message.BasicNameValuePair
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.http.HttpService


case class APIOverpass(
                        serverURL: String = APIOverpass.defaultServerURL,
                        httpService: HttpService = HttpService("OSM Overpass request")
                   )  {


  def get(south: Double, west: Double, north: Double, east: Double,
          hasKeyValue: (String,Seq[String])=("",Seq(""))): OSMRoot = {
    utils.log(s"Getting OSM data from Overpass API for bbox $east $north $south $west")

    val root = new OSMRoot
    val parser = OSMXmlParser.apply()
    parser.setRoot(root)
    val query = s"""
                   |  <query type="way">
                   |    ${if(hasKeyValue._1.length>0) "<has-kv k=\"" else ""}${hasKeyValue._1}${if(hasKeyValue._1.length>0) "\" v=\"" else ""}${hasKeyValue._2.mkString("|")}${if(hasKeyValue._1.length>0)"\"/>" else ""}
                   |    <bbox-query e="$east" n="$north" s="$south" w="$west"/>
                   |  </query>
                   |  <union>
                   |    <item />
                   |    <recurse type="way-node"/>
                   |  </union>
                   |  <print/>
           """.stripMargin
    //println(query)
    parser.parse(new StringReader(execute(query)))
    root
  }

  /**
    * 2013-07-28 Usage policy accept 10 000 requests or 5GB data per day using up to two threads.
    * See http://wiki.openstreetmap.org/wiki/Overpass_API#Introduction
    *
    * @param overpassQuery query
    * @param queryDescription query description
    * @return
    */
  @throws[APIOverpass.OverpassException]
  def execute(overpassQuery: String, queryDescription: String = null): String = try {
    val post = new HttpPost(serverURL)
    post.setHeader("User-Agent", httpService.userAgent)
    val nameValuePairs = new util.ArrayList[NameValuePair](1)
    nameValuePairs.add(new BasicNameValuePair("data", overpassQuery))
    post.setEntity(new UrlEncodedFormEntity(nameValuePairs))
    httpService.leniency()
//    println("Executing overpass query: " + queryDescription + "\n" + overpassQuery)
    //val started = System.currentTimeMillis

    val content = httpService.httpClient.execute(post).getEntity.getContent
    /*
    val res = Try {
      var content = Try{getHttpClient.execute(post).getEntity.getContent}
      while(content.isFailure){
        //  add a max number of tries ?
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
      throw APIOverpass.OverpassException(e.getMessage, e)
  }


}

object APIOverpass {

  val defaultServerURL: String = "http://www.overpass-api.de/api/interpreter"

  case class OverpassException(message: String, cause: Throwable) extends Exception(message, cause) {}

}



