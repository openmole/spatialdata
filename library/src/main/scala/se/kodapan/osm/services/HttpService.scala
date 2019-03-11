package se.kodapan.osm.services

import org.apache.http.HttpRequest
import org.apache.http.client.HttpClient
import org.apache.http.client.config.RequestConfig
import org.apache.http.conn.ClientConnectionManager
import org.apache.http.conn.scheme.PlainSocketFactory
import org.apache.http.conn.scheme.Scheme
import org.apache.http.conn.scheme.SchemeRegistry
import org.apache.http.impl.client.{DefaultHttpClient, HttpClientBuilder}
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.params.BasicHttpParams
import spatialdata.utils.http.TorPoolManager

/**
  * @author kalle
  * @since 2013-09-21 6:48 PM
  */
object HttpService {
//  private val log = LoggerFactory.getLogger(classOf[HttpService])
}

class HttpService {
  private var minimumMillisecondsDelayBetweenRequests: Long  = 0
  private var previousRequestTimestamp: Long = 0
  private var cm: ClientConnectionManager = _
  private var httpClient:HttpClient = _
  private val defaultUserAgent = "Unnamed instance of " + getClass.getName + ", https://github.com/karlwettin/osm-common/"
  private var userAgent = defaultUserAgent
  private var withTorPool: Boolean = _

  @throws[Exception]
  def open(timeout: Int=1000,torPool: Boolean=true) = {
    val schemeRegistry = new SchemeRegistry
    schemeRegistry.register(new Scheme("http", PlainSocketFactory.getSocketFactory, 80))
    cm = new ThreadSafeClientConnManager(new BasicHttpParams, schemeRegistry)
    //httpClient = new DefaultHttpClient(cm, new BasicHttpParams)
    var requestBuilder = RequestConfig.custom()
    requestBuilder.setConnectTimeout(timeout)
    requestBuilder.setConnectionRequestTimeout(timeout)
    httpClient = HttpClientBuilder.create.setDefaultRequestConfig(requestBuilder.build()).build
    withTorPool=torPool
    if(withTorPool){TorPoolManager.setupTorPoolConnexion(true)}
  }

  def setUserAgent(httpRequest: HttpRequest) = {
    if (defaultUserAgent == userAgent) throw new NullPointerException("HTTP header User-Agent not set! See se.kodapan.osm.services.HttpService#setUserAgent")
    httpRequest.setHeader("User-Agent", userAgent)
  }


  @throws[Exception]
  def close() = {
  }

  @throws[Exception]
  def leniency() = {
    var sleep = previousRequestTimestamp + minimumMillisecondsDelayBetweenRequests - System.currentTimeMillis
    while (sleep > 0) {
      Thread.sleep(sleep)
      sleep = previousRequestTimestamp + minimumMillisecondsDelayBetweenRequests - System.currentTimeMillis
    }
    previousRequestTimestamp = System.currentTimeMillis
  }

  def getUserAgent = userAgent

  def setUserAgent(userAgent: String) = {
    this.userAgent = userAgent
  }

  def getMinimumMillisecondsDelayBetweenRequests = minimumMillisecondsDelayBetweenRequests

  def setMinimumMillisecondsDelayBetweenRequests(minimumMillisecondsDelayBetweenRequests: Long) = {
    this.minimumMillisecondsDelayBetweenRequests = minimumMillisecondsDelayBetweenRequests
  }

  def getHttpClient = httpClient

  def setHttpClient(httpClient: Nothing) = {
    this.httpClient = httpClient
  }
}
