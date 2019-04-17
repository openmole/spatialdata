package org.openmole.spatialdata.network

import com.vividsolutions.jts.geom.LineString

object SpatialNetwork {

  /**
    * import a network from gis linestrings
    * @param lines
    * @param snap
    * @return
    */
  def fromGISLines(lines: Seq[LineString], snap: Double): Network = {
    /*lines.zipWithIndex.map{
      case (line,i) => line.getCoordinates.zip
    }*/
    // would require some kind of spatial index for efficiency ? -> do the snapping directly here using a hashmap with rounded coordinates
    // (quite dirty)


    Network.empty
  }


  /**
    * simplify a spatial network through snapping
    * @param network
    * @param snap
    * @return
    */
  def spatialSimplification(network: Network,snap: Double): Network = Network.empty


}
