package org.openmole.spatialdata.vector.matching

import org.openmole.spatialdata.vector.{Polygons, VectorFeatures}

trait VectorMatching {

  /**
   *
   * @param layer1 first layer
   * @param layer2 second layer
   * @param algorithmInstance algo instance with parameters
   * @return n:m matching result by feature indices
   */
  /*def matching(
                layer1: VectorFeatures,
                layer2: VectorFeatures//,
                //algorithmInstance: VectorMatching // not needed as algos extend the trait
              ): Seq[(Seq[Int], Seq[Int])]


   */
}

trait PolygonMatching extends VectorMatching {
  def matching(
                layer1: Polygons,
                layer2: Polygons//,
                //algorithmInstance: VectorMatching
              ): Seq[(Seq[Int], Seq[Int])]
}
