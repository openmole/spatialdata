package org.openmole.spatialdata.vector

import org.locationtech.jts.geom

case class Polygons(
                   polygons: Seq[geom.Polygon],
                   attributes: Seq[Attributes] = Seq.empty
                   ) {

}


object Polygons {


}
