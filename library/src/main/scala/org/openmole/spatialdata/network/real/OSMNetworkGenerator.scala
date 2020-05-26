
package org.openmole.spatialdata.network.real

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.utils.gis.{GISNetwork, GISUtils}
import org.openmole.spatialdata.utils.osm.APIExtractor
import org.openmole.spatialdata.vector.Implicits._

import scala.util.Random

/**
  *
  * @param lon window center longitude
  * @param lat window center latitude
  * @param windowSize window size in meters
  * @param tags tags to keep
  * @param simplifySnapping snapping distance
  * @param stationNodesTags tags for station nodes
  */
case class OSMNetworkGenerator(
                              lon: Double,
                              lat: Double,
                              windowSize:Double,
                              // for railway tags: https://wiki.openstreetmap.org/wiki/Key:railway
                              tags: Map[String,Seq[String]] = Map("highway"->Seq("motorway","trunk","primary","secondary","tertiary")), // unclassified
                              simplifySnapping: Double = 1e-6,
                              stationNodesTags: Option[Map[String,Seq[String]]] = None
                              ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = OSMNetworkGenerator.OSMWayNetwork(lon,lat,windowSize,simplifySnapping,tags,stationNodesTags)

}



object OSMNetworkGenerator {


  /**
    * Construct a network from OSM
    * @param lon lon
    * @param lat lat
    * @param windowSize window size
    * @param tags tags
    * @param simplifySnapping snapping
    * @return
    */
  def OSMWayNetwork(lon: Double,lat: Double,windowSize: Double,
                    simplifySnapping: Double,
                    tags: Map[String,Seq[String]],
                    stationNodesTags: Option[Map[String,Seq[String]]]
                   ): Network = {
    val (west,south,east,north)=GISUtils.wgs84window(lon,lat,windowSize)
    val lines = APIExtractor.Highways.getHighways(south,west,north,east,tags)
    val nw = GISNetwork.networkFromGISLinesGridSnapping(lines,simplifySnapping)
    utils.log("Network from OSM: "+nw)
    // note: doing a second time the request may not be optimal, but simplifies the overpass request
    if(stationNodesTags.isDefined) {
      val attributePoints = APIExtractor.Points.getPoints(south, west, north, east, stationNodesTags.get)
      GISNetwork.addStationNodes(nw, attributePoints)
    } else nw
  }


}




