package org.openmole.spatialdata

import scala.language.implicitConversions

package object vector {

  /**
    * Point in 2D
    */
  type Point = (Double,Double)


  /**
    * Spatial field
    */
  type SpatialField[N] = Map[Point,Array[N]]

  /**
    * Attributes for vector features
    */
  type Attributes = Map[String,AnyRef]

  object Attributes {
    val empty: Attributes = Map.empty[String, AnyRef]
  }

  object Implicits {

    //implicit def pointAsGeomPoint(p: Point): org.locationtech.jts.geom.Point = new org.locationtech.jts.geom.Point(Coordinate(p._1,p._2), new PrecisionModel, 0)

    implicit def linestringSeqAsLines(l: Seq[org.locationtech.jts.geom.LineString]): Lines = Lines(l,Seq.empty)

    implicit class SpatialFieldDecorator[N](s: SpatialField[N]){
      // note: not needed as the transform function already exists for maps
      //def transform(f: Array[Double] => Array[Double]): SpatialField = s.map{case (p,a) => (p,f(a))}
    }

  }


}
