package org.openmole.spatialdata.vector

import org.locationtech.jts.geom

case class Polygons(
                   polygons: Seq[geom.Polygon],
                   attributes: Seq[Attributes] = Seq.empty
                   ) {

}


object Polygons {

  def fromGeometries(geoms: Seq[geom.Geometry], attributes: Seq[Attributes]): Polygons = {
    Polygons(
      geoms.map{g =>
        //g.getGeometryType match {
        //  case
        //}
        // should split multipolygons, transform lines into polygons, etc.
        // just simple cast for now
        g.asInstanceOf[geom.Polygon]
      },
      attributes
    )
  }


}
