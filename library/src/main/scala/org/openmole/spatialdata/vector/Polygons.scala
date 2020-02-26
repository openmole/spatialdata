package org.openmole.spatialdata.vector

case class Polygons(
                   polygons: Seq[org.locationtech.jts.geom.Polygon],
                   attributes: Map[(Int, String),AnyRef]
                   ) {

}
