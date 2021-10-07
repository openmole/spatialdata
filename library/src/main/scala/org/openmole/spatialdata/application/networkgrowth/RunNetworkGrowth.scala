package org.openmole.spatialdata.application.networkgrowth

object RunNetworkGrowth extends App {

  val fuas = Seq("Limoges")

  RealData.computeRealIndicators(fuas)

}
