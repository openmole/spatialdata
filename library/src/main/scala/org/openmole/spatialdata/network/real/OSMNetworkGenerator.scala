
package org.openmole.spatialdata.network.real

import org.openmole.spatialdata.network
import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.utils.gis.GISUtils
import org.openmole.spatialdata.utils.osm.APIExtractor

import scala.util.Random

case class OSMNetworkGenerator(
                              lon: Double,
                              lat: Double,
                              windowSize:Double,
                              tags: Map[String,Seq[String]] = Map("highway"->Seq("motorway","trunk","primary","secondary","tertiary")), // unclassified
                              simplifySnapping: Double = 1e-6
                              ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = OSMNetworkGenerator.OSMWayNetwork(lon,lat,windowSize,tags,simplifySnapping)

}



object OSMNetworkGenerator {


  def OSMWayNetwork(lon: Double,lat: Double,windowSize: Double,tags: Map[String,Seq[String]],simplifySnapping: Double): Network = {
    val (west,south,east,north)=GISUtils.wgs84window(lon,lat,windowSize)
    val lines = APIExtractor.Highways.getHighways(south,west,east,north,tags)
    network.fromGISLines(lines,simplifySnapping)
  }


}




