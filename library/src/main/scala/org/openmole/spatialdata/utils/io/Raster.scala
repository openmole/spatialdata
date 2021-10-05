package org.openmole.spatialdata.utils.io


import java.io.File

import javax.imageio.ImageIO
import org.geotools.coverage.grid.io.{AbstractGridFormat, GridCoverage2DReader, GridFormatFinder, OverviewPolicy}
import org.geotools.gce.geotiff.GeoTiffReader
import org.geotools.parameter.ParameterGroup
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
  def readGeotiff(layer: String): RasterLayerData[Double] = {
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

    val raw = reader.read(Array(policy,gridsize,useJaiRead))

    val raster: java.awt.image.Raster = raw.getRenderedImage.getData
    val res: ArrayBuffer[Array[Double]] = new ArrayBuffer
    for {x <- raster.getMinX to (raster.getMinX + raster.getWidth)} {
       val currentData = new Array[Double](raster.getHeight)
       res.addOne(raster.getPixels(x,raster.getMinY,1,raster.getHeight,currentData))
    }
    res.toArray
  }

}
