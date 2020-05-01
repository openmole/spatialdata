package org.openmole.spatialdata.utils.http

import org.apache.http.HttpRequest
import org.apache.http.client.HttpClient
import org.apache.http.client.config.RequestConfig
import org.apache.http.impl.client.HttpClientBuilder



case class HttpService(
                        userAgent: String,
                        httpClient: HttpClient,
                        minimumMillisecondsDelayBetweenRequests: Long,
                        timeout: Int
                      ) {
  private var previousRequestTimestamp: Long = 0L

  def setUserAgent(httpRequest: HttpRequest): Unit = {
    httpRequest.setHeader("User-Agent", userAgent)
  }


  @throws[Exception]
  def leniency(): Unit = {
    var sleep = previousRequestTimestamp + minimumMillisecondsDelayBetweenRequests - System.currentTimeMillis
    while (sleep > 0) {
      Thread.sleep(sleep)
      sleep = previousRequestTimestamp + minimumMillisecondsDelayBetweenRequests - System.currentTimeMillis
    }
    previousRequestTimestamp = System.currentTimeMillis
  }

}

object HttpService {
  val defaultUserAgent: String = ""

  def apply(userAgent: String = defaultUserAgent, withTorPool: Boolean = false, minimumMillisecondsDelayBetweenRequests: Long = 0L, timeout: Int = 1000): HttpService = {
    val requestBuilder = RequestConfig.custom()
    requestBuilder.setConnectTimeout(timeout)
    requestBuilder.setConnectionRequestTimeout(timeout)
    if(withTorPool){TorPoolManager.setupTorPoolConnexion(true)}
    HttpService(userAgent = userAgent, httpClient = HttpClientBuilder.create.setDefaultRequestConfig(requestBuilder.build()).build, minimumMillisecondsDelayBetweenRequests, timeout)
  }
}
