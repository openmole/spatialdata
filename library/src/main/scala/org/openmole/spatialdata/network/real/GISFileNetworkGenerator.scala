package org.openmole.spatialdata.network.real

import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.utils.gis.{GISNetwork, GeometryUtils}
import org.openmole.spatialdata.utils.io.{GIS, GeoPackage, Shapefile}
import org.openmole.spatialdata.vector.{Attributes, Lines}
import org.locationtech.jts.geom
import org.openmole.spatialdata.utils

import scala.util.Random

/**
  * Construct a network from gis files
  * @param files files paths
  * @param simplifySnapping snapping
  */
case class GISFileNetworkGenerator(
                               files: Seq[String],
                               simplifySnapping: Double = 1e-6,
                               weightAttribute: String = "",
                               mask: Option[Either[geom.Geometry,String]] = None
                               ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = GISFileNetworkGenerator.GISFileNetwork(files, simplifySnapping, weightAttribute, mask)

}


object GISFileNetworkGenerator {

  /**
    *
    * @param files files
    * @param simplifySnapping snapping
    * @param weightAttribute weight attribute
    * @param mask option for a spatial mask: Either Geometry, or GIS file assumed to contain a single geometry
    * @return
    */
  def GISFileNetwork(files: Seq[String], simplifySnapping: Double, weightAttribute: String = "", mask: Option[Either[geom.Geometry,String]] = None): Network = {
    val lines: Lines = files.map{file =>
      GIS.readGeometry(file).map{g => GeometryUtils.geometryToLines(g._1)}.reduce(Lines.++)
    }.reduce(Lines.++)
    val filteredlines = if (mask.isEmpty) lines else {
      val maskarea: geom.Geometry = mask.get match {case Left(p) =>p; case Right(s) => GIS.readGeometry(s)(0)._1}
      lines.filter(_._1.intersects(maskarea))
    }
    utils.log("filtered lines: "+filteredlines.lines.size)
    GISNetwork.networkFromGISLinesGridSnapping(filteredlines,simplifySnapping, weightAttribute = weightAttribute)
  }

}
