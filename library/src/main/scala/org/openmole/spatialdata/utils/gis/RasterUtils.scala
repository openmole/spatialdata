package org.openmole.spatialdata.utils.gis

import org.geotools.coverage.grid.{GridCoordinates2D, GridCoverage2D}
import org.geotools.coverage.processing.Operations
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.vector.{Attributes, Points}
import org.locationtech.jts.geom

import scala.collection.mutable.ArrayBuffer

object RasterUtils {

  val op: Operations = Operations.DEFAULT

  /*def cropRaster(raster: GridCoverage2D, xmin: Double, xmax: Double, ymin: Double, ymax: Double): GridCoverage2D = {
    op.crop(new Envelope(xmin, xmax, ymin, ymax))
  }*/

  def cropRaster(raster: GridCoverage2D, envelope: Geometry): GridCoverage2D = op.crop(raster, envelope).asInstanceOf[GridCoverage2D]

  /**
    * extract one band as raw matrix
    * @param coverage coverage
    * @param band band
    * @return
    */
  def gridCoverage2DAsRawData(coverage: GridCoverage2D, band: Int = 0): Array[Array[Double]] = {
    val raster = coverage.getRenderedImage.getData
    val rawtmp = Array.fill(raster.getWidth*raster.getHeight)(0.0)
    //utils.log(s"Raster coords: xmin = ${raster.getMinX} ; ymin = ${raster.getMinY} ; width = ${raster.getWidth} ; height = ${raster.getHeight}")
    val raw = raster.getSamples(raster.getMinX,raster.getMinY,raster.getWidth , raster.getHeight, band,rawtmp)
    raw.grouped(raster.getWidth).toArray
  }

  /**
    * extract one band as Points
    * @param coverage coverage
    * @param band band
    * @param attrPrefix attribute prefix (band number appended)
    * @return
    */
  def gridCoverage2DAsPoints(coverage: GridCoverage2D, band: Int = 0, attrPrefix: String = "v"): Points = {
    val raster: java.awt.image.Raster = coverage.getRenderedImage.getData
    val rastergeom = coverage.getGridGeometry
    val (v,xx,yy) = (new ArrayBuffer[Double], new ArrayBuffer[Double], new ArrayBuffer[Double])
    for {x <- raster.getMinX to (raster.getMinX + raster.getWidth); y <- raster.getMinY to (raster.getMinY + raster.getHeight)} {
      v.addOne(raster.getSampleDouble(x,y,band))
      val p = rastergeom.gridToWorld(new GridCoordinates2D(x,y)).getCoordinate
      xx.addOne(p(0));yy.addOne(p(1))
    }

    val factory = new GeometryFactory

    Points(
      xx.toSeq.zip(yy.toSeq).map{case (x,y) => factory.createPoint(new Coordinate(x,y))},
      v.toSeq.map{(d: Double) => Map(attrPrefix+band.toString -> d).asInstanceOf[Attributes]}
    )
  }


}
