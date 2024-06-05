package org.openmole.spatialdata.utils.io


import java.io.File
import org.geotools.coverage.grid.{GridCoordinates2D, GridCoverage2D}
import org.geotools.coverage.grid.io.{AbstractGridFormat, GridFormatFinder, OverviewPolicy}
import org.geotools.gce.geotiff.GeoTiffReader
import org.opengis.parameter.{GeneralParameterValue, ParameterValue}
import org.openmole.spatialdata.grid.RasterLayerData
import org.openmole.spatialdata.utils

import scala.collection.mutable.ArrayBuffer

object Raster {

  /**
    * Read raw data from a raster
    *
    * See  https://gis.stackexchange.com/questions/314421/geotiff-geotools-mollweide-customer-projection-not-recognized
    * https://gis.stackexchange.com/questions/106882/reading-each-pixel-of-each-band-of-multiband-geotiff-with-geotools-java?rq=1
    *   -> prj file needed although when metadata within the tif - use directly GeoTiffReader
    *
    * @param layer raster file
    * @return
    */
  def readGeotiff(layer: String): GridCoverage2D = {
    val format: AbstractGridFormat = GridFormatFinder.findFormat(layer)
    utils.log(s"Raster format for file $layer: ${format.getDescription}")
    //val reader: GridCoverage2DReader = format.getReader(layer)
    //println(ImageIO.read(new File(layer)).getRaster.getSampleDouble(10,10,0))

    val reader = new GeoTiffReader(new File(layer))
    //println(reader)
    //val raw = reader.read(Array(org.geotools.parameter.Parameter.create("dummy",0).asInstanceOf[org.opengis.parameter.GeneralParameterValue]))
    //println(raw)
    val policy: ParameterValue[OverviewPolicy] = AbstractGridFormat.OVERVIEW_POLICY.createValue
    policy.setValue(OverviewPolicy.IGNORE)
    val gridsize: ParameterValue[String] = AbstractGridFormat.SUGGESTED_TILE_SIZE.createValue
    val useJaiRead = AbstractGridFormat.USE_JAI_IMAGEREAD.createValue
    useJaiRead.setValue(true)

    val params: Array[GeneralParameterValue] = Array(policy,gridsize,useJaiRead)

    reader.read(params)
  }


  /**
    * Get one band of a geotiff raster as a raw matrix
    * @param layer layer
    * @param band band (default 0)
    * @return
    */
  def readGeotiffValues(layer: String, band: Int = 0): Array[Double] = {
    val raw: GridCoverage2D = readGeotiff(layer)
    val raster: java.awt.image.Raster = raw.getRenderedImage.getData
    val res = Array.fill(raster.getWidth*raster.getHeight)(0.0)
    raster.getSamples(0,0,raster.getWidth, raster.getHeight,band,res)
  }

  /**
    * get one band and coordinates of cells for a geotiff raster
    * @param layer layer
    * @param band band (default 0)
    * @return
    */
  def readGeotiffValuesAndCoordinates(layer: String, band: Int = 0): (Array[Double], Array[Double], Array[Double]) = {
    val raw: GridCoverage2D = readGeotiff(layer)
    val raster: java.awt.image.Raster = raw.getRenderedImage.getData
    val rastergeom = raw.getGridGeometry
    val (v,xx,yy) = (new ArrayBuffer[Double], new ArrayBuffer[Double], new ArrayBuffer[Double])
    for {x <- raster.getMinX to (raster.getMinX + raster.getWidth); y <- raster.getMinY to (raster.getMinY + raster.getHeight)} {
      v.addOne(raster.getSampleDouble(x,y,band))
      val p = rastergeom.gridToWorld(new GridCoordinates2D(x,y)).getCoordinate
      xx.addOne(p(0));yy.addOne(p(1))
    }
    (v.toArray, xx.toArray, yy.toArray)
  }

}
