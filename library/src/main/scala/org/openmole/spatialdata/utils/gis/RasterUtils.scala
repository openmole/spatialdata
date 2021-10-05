package org.openmole.spatialdata.utils.gis

import org.geotools.coverage.grid.GridCoverage2D
import org.geotools.coverage.processing.{CoverageProcessor, Operations}
import org.opengis.geometry.Envelope
import org.opengis.parameter.ParameterValueGroup

object RasterUtils {

  val op: Operations = Operations.DEFAULT

  /*def cropRaster(raster: GridCoverage2D, xmin: Double, xmax: Double, ymin: Double, ymax: Double): GridCoverage2D = {
    op.crop(new Envelope(xmin, xmax, ymin, ymax))
  }*/

  def cropRaster(raster: GridCoverage2D, envelope: Envelope): GridCoverage2D = op.crop(raster, envelope).asInstanceOf[GridCoverage2D]

}
