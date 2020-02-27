package org.openmole.spatialdata.vector


import scala.util.Random

/**
  * Spatial field generator
  *
  * Note: should generalize a spatial field to any VectorFeatures? (or stick with values at centroids for polygons)
  *  - or at least provide implicit conversion utils
  *  -> Points.asSpatialField does this for points explicitly already
  *
  * @tparam N type of the field
  */
trait FieldGenerator[N] {

  /**
    * generate the field
    * @param rng rng
    * @return field
    */
  def generateField(implicit rng: Random): SpatialField[N]
}
