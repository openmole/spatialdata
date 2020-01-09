
package org.openmole.spatialdata.network.real

import org.openmole.spatialdata.{network, utils}
import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.utils.gis.{GISNetwork, GISUtils}
import org.openmole.spatialdata.utils.osm.api.APIExtractor

import scala.util.Random

case class OSMNetworkGenerator(
                              lon: Double,
                              lat: Double,
                              windowSize:Double,
                              tags: Map[String,Seq[String]] = Map("highway"->Seq("motorway","trunk","primary","secondary","tertiary")), // unclassified
                              simplifySnapping: Double = 1e-6
                              ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = OSMNetworkGenerator.OSMWayNetwork(lon,lat,windowSize,simplifySnapping,tags)

}



object OSMNetworkGenerator {


  /**
    * Construct a network from OSM
    * @param lon
    * @param lat
    * @param windowSize
    * @param tags
    * @param simplifySnapping
    * @return
    */
  def OSMWayNetwork(lon: Double,lat: Double,windowSize: Double, simplifySnapping: Double,
                    tags: Map[String,Seq[String]]
                   ): Network = {
    val (west,south,east,north)=GISUtils.wgs84window(lon,lat,windowSize)
    val lines = APIExtractor.Highways.getHighways(south,west,north,east,tags)
    val nw = GISNetwork.networkFromGISLinesGridSnapping(lines,simplifySnapping)
    utils.log("Network from OSM: "+nw)
    nw
  }


}




