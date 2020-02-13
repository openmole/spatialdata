package org.openmole.spatialdata.vector

import org.openmole.spatialdata.vector.Point

/**
  * A spatial points data frame
  *
  * @param points
  * @param attributes
  */
case class Points(
                 points: Seq[org.locationtech.jts.geom.Point],
                 attributes: Map[(Int, String),AnyRef]
                 )


object Points {

  val empty = Points(Seq.empty[org.locationtech.jts.geom.Point], Map.empty)

  /*
  def apply(points: Seq[Point],attributes: Map[(Int, String),AnyRef]): Points = {
    import org.openmole.spatialdata.vector.Implicits._
    Points(points,attributes)
  }
  */



}
