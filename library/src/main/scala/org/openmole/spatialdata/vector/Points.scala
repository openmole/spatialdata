package org.openmole.spatialdata.vector

import org.locationtech.jts.geom
import org.locationtech.jts.geom.{Coordinate, GeometryCollection, GeometryFactory}

import scala.reflect.ClassTag

/**
  * A spatial points data frame
  *
  * @param points points
  * @param attributes attributes
  */
case class Points(
                 points: Seq[geom.Point],
                 attributes: Seq[Attributes]
                 ) extends VectorFeatures {

  /**
    * convert the feature to a spatial field
    *  Note: all attributes should be of the same type T (unsafe casting)
    * @tparam T type of the field (attributes unsafely casted into it)
    * @return
    */
  def asSpatialField[T: ClassTag](defaultAttribute: T): SpatialField[T] = {
    val attributesNames = attributes.head.keys.toArray // assumes all features have all attributes names
    points.zip(attributes).map{case (p,attr) =>
      ((p.getCoordinate.x,p.getCoordinate.y),attributesNames.map(s => attr.getOrElse(s,defaultAttribute).asInstanceOf[T]))
    }.toMap
  }

  /**
    * simple point seq
    * @return point seq
    */
  def asPointSeq: Seq[Point] = points.map(p => (p.getCoordinate.x,p.getCoordinate.y))

  /**
   * convert to jts GeometryCollection
   * @return
   */
  def asGeometryCollection: GeometryCollection = {
    val fact = new GeometryFactory
    fact.createGeometryCollection(points.toArray)
  }

}


object Points {

  val empty: Points = Points(Seq.empty[org.locationtech.jts.geom.Point], Seq.empty)


  /**
    * construct from points (can not be apply, same type than case class constructor)
    * @param points points
    * @param attributes atrributes
    * @return
    */
  def fromPoints(points: Iterable[Point],attributes:Seq[Attributes] = Seq.empty): Points = {
    val factory = new GeometryFactory
    Points(
      points.map{p => factory.createPoint(new Coordinate(p._1,p._2))}.toSeq,
      attributes
    )
  }



}
