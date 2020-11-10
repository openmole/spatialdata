package org.openmole.spatialdata.network.real

import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.utils.gis.{GISNetwork, GeometryUtils}
import org.openmole.spatialdata.utils.io.GIS
import org.openmole.spatialdata.vector.Lines
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
                               mask: Option[Either[geom.Geometry,String]] = None,
                               reproject: Option[Lines => Lines] = None
                               ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = GISFileNetworkGenerator.GISFileNetwork(files, simplifySnapping, weightAttribute, mask, reproject)

}


object GISFileNetworkGenerator {

  implicit val ordering: Ordering[Double] = Ordering.Double.TotalOrdering

  /**
    *
    * @param files files
    * @param simplifySnapping snapping
    * @param weightAttribute weight attribute
    * @param mask option for a spatial mask: Either Geometry, or GIS file assumed to contain a single geometry
    * @param reproject optional reprojection of lines BEFORE masking
    * @return
    */
  def GISFileNetwork(files: Seq[String],
                     simplifySnapping: Double,
                     weightAttribute: String = "",
                     mask: Option[Either[geom.Geometry,String]] = None,
                     reproject: Option[Lines => Lines] = None
                    ): Network = {
    val lines: Lines = files.map{file =>
      GIS.readGeometry(file).map{g => GeometryUtils.geometryToLines(g._1)}.reduce(Lines.++)
    }.reduce(Lines.++)
    val trlines = if (reproject.isEmpty) lines else reproject.get.apply(lines)
    //utils.log("xmin = "+trlines.lines.map(_.getEnvelopeInternal.getMinX).min+" ; xmax = "+trlines.lines.map(_.getEnvelopeInternal.getMaxX).max+" ; ymin = "+trlines.lines.map(_.getEnvelopeInternal.getMinY).min+" ; ymax = "+trlines.lines.map(_.getEnvelopeInternal.getMaxY).max)

    val filteredlines = if (mask.isEmpty) trlines else {
      val maskarea: geom.Geometry = mask.get match {case Left(p) =>p; case Right(s) => GIS.readGeometry(s)(0)._1}
      //utils.log("Mask: "+maskarea.getEnvelopeInternal.toString+" ; "+maskarea.toString)
      //utils.log(trlines.lines.take(2).map(_.toString).mkString("\n"))
      //utils.log(trlines.lines.size.toString)
      /*utils.log(trlines.lines.zip(trlines.attributes).map { case (l, _) => l.getEnvelopeInternal.getMinX > maskarea.getEnvelopeInternal.getMinX &&
        l.getEnvelopeInternal.getMinY > maskarea.getEnvelopeInternal.getMinY &&
        l.getEnvelopeInternal.getMaxX < maskarea.getEnvelopeInternal.getMaxX &&
        l.getEnvelopeInternal.getMaxY < maskarea.getEnvelopeInternal.getMaxY
      }.mkString(""))*/
      //trlines.filter(l => l._1.intersects(maskarea)||maskarea.contains(l._1)) // why does this not work?
      val filt = trlines.lines.zipWithIndex.filter {case (l,i) =>
          l.getEnvelopeInternal.getMinX > maskarea.getEnvelopeInternal.getMinX &&
          l.getEnvelopeInternal.getMinY > maskarea.getEnvelopeInternal.getMinY &&
          l.getEnvelopeInternal.getMaxX < maskarea.getEnvelopeInternal.getMaxX &&
          l.getEnvelopeInternal.getMaxY < maskarea.getEnvelopeInternal.getMaxY
      }
      // Lines filter also does not work
      val inds = filt.map(_._2)
      //utils.log(inds.toString)
      Lines(filt.map(_._1),trlines.attributes.zipWithIndex.filter{case (_,i) => inds.contains(i)}.map(_._1))
    }
    utils.log("filtered lines: "+filteredlines.lines.size)
    GISNetwork.networkFromGISLinesGridSnapping(filteredlines,simplifySnapping, weightAttribute = weightAttribute)
  }

}
