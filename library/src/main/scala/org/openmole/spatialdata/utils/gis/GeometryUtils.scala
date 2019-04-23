package org.openmole.spatialdata.utils.gis

import com.vividsolutions.jts.geom.{Geometry, GeometryFactory}
import org.openmole.spatialdata._

object GeometryUtils {

  /**
    * Get the centroid of the convex hull of a point cloud
    * @param pi
    * @return
    */
  def convexHullCentroid(pi: Array[Point2D]): Point2D = {
    val convexHullCentroid = convexHullPoints(pi).getCentroid
     (convexHullCentroid.getX,convexHullCentroid.getY)
  }

  def convexHullArea(pi: Array[Point2D]): Double = convexHullPoints(pi).getArea

  def convexHullPoints(pi: Array[Point2D]): Geometry = {
    val geomFactory = new GeometryFactory
    geomFactory.createMultiPoint(pi.map{case (x,y)=>geomFactory.createPoint(new com.vividsolutions.jts.geom.Coordinate(x,y))}).convexHull
  }


}
