package org.openmole.spatialdata.vector

import org.locationtech.jts.geom

case class Polygons(
                   polygons: Seq[geom.Polygon],
                   attributes: Map[(Int, String),AnyRef] = Map.empty[(Int, String),AnyRef]
                   ) {

}


object Polygons {


}
