
package spatialdata.network

import com.vividsolutions.jts.geom.LineString


object SpatialNetwork {

  def fromGISLines(lines: Seq[LineString], snap: Double): Network = {
    /*lines.zipWithIndex.map{
      case (line,i) => line.getCoordinates.zip
    }*/
    // would require some kind of spatial index for efficiency ? -> do the snapping directly here using a hashmap with rounded coordinates
    // (quite dirty)


    Network.empty
  }


  def spatialSimplification(network: Network,snap: Double): Network = Network.empty


}
