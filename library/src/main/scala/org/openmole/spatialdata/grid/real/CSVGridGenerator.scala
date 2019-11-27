package org.openmole.spatialdata.grid.real

import org.openmole.spatialdata.RasterLayerData
import org.openmole.spatialdata.grid.GridGenerator
import org.openmole.spatialdata.utils.io.CSV

import scala.util.Random

case class CSVGridGenerator(
                           file: String,
                           separator: String = ","
                           ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = CSV.readMat(file,separator,naformat = "NA")

}
