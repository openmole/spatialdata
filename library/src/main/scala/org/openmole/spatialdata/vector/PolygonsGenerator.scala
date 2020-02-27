package org.openmole.spatialdata.vector

import scala.util.Random

/**
  * Generic polygon generator
  */
trait PolygonsGenerator {

  def generatePolygons(implicit rng: Random): Polygons

}
