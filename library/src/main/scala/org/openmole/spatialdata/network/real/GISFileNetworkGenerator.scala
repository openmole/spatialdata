package org.openmole.spatialdata.network.real

import org.locationtech.jts.geom
import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.utils.gis.{GISNetwork, GeometryUtils}
import org.openmole.spatialdata.utils.io.Shapefile
import org.openmole.spatialdata.vector.Lines

import scala.util.Random

case class GISFileNetworkGenerator(
                               file: String,
                               simplifySnapping: Double = 1e-6,
                               format: String = "shp"
                               ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = GISFileNetworkGenerator.GISFileNetwork(file, simplifySnapping, format)

}


object GISFileNetworkGenerator {

  def GISFileNetwork(file: String, simplifySnapping: Double, format: String): Network = {
    val lines = format match {
      case s if s == "shp" => Shapefile.readGeometry(file).map{g => GeometryUtils.geometryToLines(g._1)}.reduce(Lines.++)
      case _ => Lines.empty
    }
    GISNetwork.networkFromGISLinesGridSnapping(lines,simplifySnapping)
  }

}
