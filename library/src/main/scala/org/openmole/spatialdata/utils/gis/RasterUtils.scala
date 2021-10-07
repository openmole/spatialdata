package org.openmole.spatialdata.utils.gis

import org.geotools.coverage.grid.GridCoverage2D
import org.geotools.coverage.processing.Operations
import org.locationtech.jts.geom.Geometry
import org.openmole.spatialdata.utils

object RasterUtils {

  val op: Operations = Operations.DEFAULT

  /*def cropRaster(raster: GridCoverage2D, xmin: Double, xmax: Double, ymin: Double, ymax: Double): GridCoverage2D = {
    op.crop(new Envelope(xmin, xmax, ymin, ymax))
  }*/

  def cropRaster(raster: GridCoverage2D, envelope: Geometry): GridCoverage2D = op.crop(raster, envelope).asInstanceOf[GridCoverage2D]

  /**
    * extract first band as raw matrix
    * @param coverage coverage
    * @return
    */
  def gridCoverage2DAsRawData(coverage: GridCoverage2D): Array[Array[Double]] = {
    val raster = coverage.getRenderedImage.getData
    val rawtmp = Array.fill(raster.getWidth*raster.getHeight)(0.0)
    //utils.log(s"Raster coords: xmin = ${raster.getMinX} ; ymin = ${raster.getMinY} ; width = ${raster.getWidth} ; height = ${raster.getHeight}")
    val raw = raster.getSamples(raster.getMinX,raster.getMinY,raster.getWidth , raster.getHeight,0,rawtmp)
    raw.grouped(raster.getWidth).toArray
  }


}
