package org.openmole.spatialdata.vector.matching
import org.openmole.spatialdata.vector.{Polygons, VectorFeatures}


/**
 * reimplementation of the geoxygen version:
 *   https://ignf.github.io/geoxygene/documentation/application/matching-surface.html
 *   https://github.com/IGNF/geoxygene/blob/master/geoxygene-contrib/src/main/java/fr/ign/cogit/geoxygene/contrib/appariement/surfaces/AppariementSurfaces.java
 */
case class BelHadjAliMatching(

                             ) extends PolygonMatching {
  override def matching(layer1: Polygons, layer2: Polygons): Seq[(Seq[Int], Seq[Int])] = BelHadjAliMatching.matching(layer1, layer2, this)

  //override def matching(layer1: VectorFeatures, layer2: VectorFeatures): Seq[(Seq[Int], Seq[Int])] = ???
}

object BelHadjAliMatching {

  /**
   *
   * @param layer1
   * @param layer2
   * @param algorithmInstance
   * @return
   */
  def matching(layer1: Polygons, layer2: Polygons, algorithmInstance: BelHadjAliMatching): Seq[(Seq[Int], Seq[Int])] = {



    Seq.empty[(Seq[Int], Seq[Int])]
  }

}

