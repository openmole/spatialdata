package org.openmole.spatialdata

import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geom.Coordinate

package object vector {

  /**
    * Point in 2D
    */
  type Point = (Double,Double)


  /**
    * Spatial field
    */
  type SpatialField = Map[Point,Array[Double]]


  object Implicits {

    //implicit def pointAsGeomPoint(p: Point): org.locationtech.jts.geom.Point = new org.locationtech.jts.geom.Point(Coordinate(p._1,p._2), new PrecisionModel, 0)

    implicit def linestringSeqAsLines(l: Seq[org.locationtech.jts.geom.LineString]): Lines = Lines(l,Map.empty)

  }


}
