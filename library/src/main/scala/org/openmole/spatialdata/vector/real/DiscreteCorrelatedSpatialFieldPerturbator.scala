package org.openmole.spatialdata.vector.real

import org.openmole.spatialdata.vector.SpatialField

import scala.util.Random

case class DiscreteCorrelatedSpatialFieldPerturbator(

                                                    ) extends SpatialFieldPerturbator {

  override def perturbateSpatialField(field: SpatialField[Double])(implicit rng: Random): SpatialField[Double] = field

}
