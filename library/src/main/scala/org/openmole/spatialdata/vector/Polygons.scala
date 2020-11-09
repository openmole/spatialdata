package org.openmole.spatialdata.vector

import org.locationtech.jts.geom
import org.locationtech.jts.geom.GeometryFactory

case class Polygons(
                   polygons: Seq[geom.Polygon],
                   attributes: Seq[Attributes] = Seq.empty
                   ) {

  implicit val ordering: Ordering[Double] = Ordering.Double.TotalOrdering

  /**
    * get the union of bounding boxes of all polygons
    * @return
    */
  def getEnvelope: geom.Polygon = {
    val (xs,ys) = polygons.map(_.getEnvelopeInternal).map(e => ((e.getMinX,e.getMaxX),(e.getMinY,e.getMaxY))).unzip
    val (minXs,maxXs) = xs.unzip
    val (minYs,maxYs) = ys.unzip
    val (xmin,xmax,ymin,ymax) = (minXs.min,maxXs.max,minYs.min,maxYs.max)
    (new GeometryFactory).createPolygon(Array(new geom.Coordinate(xmin,ymin),new geom.Coordinate(xmax,ymin),new geom.Coordinate(xmax,ymax),new geom.Coordinate(xmin,ymax),new geom.Coordinate(xmin,ymin)))
  }

}


object Polygons {

  /**
    * Construct Polygons from geometries and attributes
    * @param geoms geometries
    * @param attributes attributes
    * @return
    */
  def fromGeometries(geoms: Seq[geom.Geometry], attributes: Seq[Attributes]): Polygons = {
    Polygons(
      geoms.map{g =>
        g.getGeometryType match {
          case "Polygon" => g.asInstanceOf[geom.Polygon]
          case "MultiPolygon" => g.asInstanceOf[geom.MultiPolygon].getGeometryN(0).asInstanceOf[geom.Polygon]
          case s => throw new UnsupportedOperationException("Conversion of geometry "+s+" to Polygons is not supported")
        }
        // should split multipolygons, transform lines into polygons, etc. - basic behavior for now: first of multi
      },
      attributes
    )
  }


}
