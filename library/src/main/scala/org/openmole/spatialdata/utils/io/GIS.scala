package org.openmole.spatialdata.utils.io

import org.locationtech.jts.geom.Geometry
import org.openmole.spatialdata.grid.RasterLayerData
import org.openmole.spatialdata.vector.Attributes


/*
// no use?
trait GIS {
  def readGeometry(layer: String,attributes:Array[String]=Array.empty): Seq[(Geometry,Attributes)]
  def readRaster(layer: String): RasterLayerData[Double]
}
*/

/**
  * Dispatch gis files into different readers
  */
object GIS {

  def readGeometry(layer: String,attributes:Array[String]=Array.empty): Seq[(Geometry,Attributes)] = {
    layer.split("\\.").last match {
      case "shp" => Shapefile.readGeometry(layer, attributes)
      case "gpkg" => GeoPackage.readGeometry(layer, attributes = attributes)
      case _ => throw new UnsupportedOperationException("Unknown GIS format")
    }
  }

  def readRaster(layer: String): RasterLayerData[Double] = {
    layer.split("\\.").last match {
      case s if s.equals("tif")||s.equals(".tiff") =>Raster.readGeotiff(layer)
      case _ => throw new UnsupportedOperationException("Unknown raster format")
    }
  }

}
