package org.openmole.spatialdata.vector

import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Coordinate
import org.openmole.spatialdata.vector.Point

import scala.reflect.ClassTag

/**
  * A spatial points data frame
  *
  * @param points
  * @param attributes
  */
case class Points(
                 points: Seq[org.locationtech.jts.geom.Point],
                 attributes: Map[(Int, String),AnyRef]
                 ) {

  /**
    * convert the feature to a spatial field
    *  Note: all attributes should be of the same type T (unsafe casting)
    * @tparam T
    * @return
    */
  def asSpatialField[T: ClassTag](defaultAttribute: T): SpatialField[T] = {
    val attributesNames = attributes.keys.map(_._2).toSet.toArray
    points.zipWithIndex.map{case (p,i) =>
      ((p.getCoordinate.x,p.getCoordinate.y),attributesNames.map(s => attributes.getOrElse((i,s),defaultAttribute).asInstanceOf[T]))
    }.toMap
  }

}


object Points {

  val empty = Points(Seq.empty[org.locationtech.jts.geom.Point], Map.empty)


  /**
    * construct from points (can not be apply, same type than case class constructor)
    * @param points
    * @param attributes
    * @return
    */
  def fromPoints(points: Seq[Point],attributes: Map[(Int, String),AnyRef]): Points = {
    val factory = new GeometryFactory
    Points(
      points.map{p => factory.createPoint(new Coordinate(p._1,p._2))},
      attributes
    )
  }




}
