
package org.openmole.spatialdata.network.real

import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.utils.gis.{GISNetwork, GISUtils}
import org.openmole.spatialdata.utils.osm.APIExtractor
import org.openmole.spatialdata.utils.osm.APIExtractor.OSMOverpass
import org.openmole.spatialdata.vector.Implicits._

import scala.util.Random

/**
  * Construct a network from OpenStreetMap
  *   api.openstreetmap.org
  *
  * @param lonLatSize window center longitude, lat and size in meters
  * @param bbox (Double, Double, Double, Double) not used if not provided
  * @param tags tags to keep
  * @param simplifySnapping snapping distance
  * @param stationNodesTags tags for station nodes
  */
case class OSMNetworkGenerator(
                              lonLatSize: (Double, Double, Double) = (0.0,0.0,0.0),
                              bbox: (Double, Double, Double, Double) = (0.0,0.0,0.0,0.0),
                              // for railway tags: https://wiki.openstreetmap.org/wiki/Key:railway
                              tags: Map[String,Seq[String]] = Map("highway"->Seq("motorway","trunk","primary","secondary","tertiary","unclassified")),
                              simplifySnapping: Double = 1e-6,
                              stationNodesTags: Option[Map[String,Seq[String]]] = None
                              ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = {
    val (lon,lat,windowSize) = lonLatSize
    OSMNetworkGenerator.OSMWayNetwork(lon,lat,windowSize,bbox,simplifySnapping,tags,stationNodesTags)
  }

}



object OSMNetworkGenerator {

  def apply(lon: Double, lat: Double, size: Double, simplifySnapping: Double): OSMNetworkGenerator = OSMNetworkGenerator(lonLatSize = (lon,lat,size), simplifySnapping = simplifySnapping)

  /**
    * Construct a network from OSM
    * @param lon lon
    * @param lat lat
    * @param windowSize window size
    * @param bbox (south,west,north,east)
    * @param tags tags
    * @param simplifySnapping snapping
    * @return
    */
  def OSMWayNetwork(lon: Double,
                    lat: Double,
                    windowSize: Double,
                    bbox: (Double, Double, Double, Double),
                    simplifySnapping: Double,
                    tags: Map[String,Seq[String]],
                    stationNodesTags: Option[Map[String,Seq[String]]]
                   ): Network = {
    val (west,south,east,north)= if (bbox._1!=0.0&&bbox._2!=0.0&&bbox._3!=0.0&&bbox._4!=0.0) bbox else GISUtils.wgs84window(lon,lat,windowSize)
    // tags query works with overpass only
    val lines = APIExtractor.Highways.getHighways(south,west,north,east,tags, mode = OSMOverpass)
    val nw = GISNetwork.networkFromGISLinesGridSnapping(lines,simplifySnapping)
    //utils.log("Network from OSM: "+nw)
    // note: doing a second time the request may not be optimal, but simplifies the overpass request
    if(stationNodesTags.isDefined) {
      val attributePoints = APIExtractor.OSMPoints.getPoints(south, west, north, east, stationNodesTags.get)
      GISNetwork.addStationNodes(nw, attributePoints)
    } else nw
  }


}




