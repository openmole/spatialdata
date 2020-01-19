package org.openmole.spatialdata.utils.gis

import org.locationtech.jts.geom.{Geometry, GeometryFactory}
import org.openmole.spatialdata.vector._

object GeometryUtils {

  /**
    * Get the centroid of the convex hull of a point cloud
    * @param pi
    * @return
    */
  def convexHullCentroid(pi: Array[Point]): Point = {
    val convexHullCentroid = convexHullPoints(pi).getCentroid
     (convexHullCentroid.getX,convexHullCentroid.getY)
  }

  /**
    * Get the area of the convex hull of a set of points
    * @param pi
    * @return
    */
  def convexHullArea(pi: Array[Point]): Double = convexHullPoints(pi).getArea

  /**
    * Get the convex hull of a set of points
    * @param pi
    * @return
    */
  def convexHullPoints(pi: Array[Point]): Geometry = {
    val geomFactory = new GeometryFactory
    geomFactory.createMultiPoint(pi.map{case (x,y)=>geomFactory.createPoint(new org.locationtech.jts.geom.Coordinate(x,y))}).convexHull
  }


}
