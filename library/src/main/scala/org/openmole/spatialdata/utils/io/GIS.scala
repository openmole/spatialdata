package org.openmole.spatialdata.utils.io

import org.locationtech.jts.geom.Geometry
import org.openmole.spatialdata.vector.Attributes


trait GIS {
  def readGeometry(layer: String,attributes:Array[String]=Array.empty): Seq[(Geometry,Attributes)]
}

/**
  * Dispatch gis files into different readers
  */
object GIS extends GIS {

  def readGeometry(layer: String,attributes:Array[String]=Array.empty): Seq[(Geometry,Attributes)] = {
    layer.split("\\.").last match {
      case "shp" => Shapefile.readGeometry(layer)
      case "gpkg" => GeoPackage.readGeometry(layer)
      case _ => throw new UnsupportedOperationException("Unknown GIS format")
    }
  }

}
