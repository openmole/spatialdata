package org.openmole.spatialdata.vector.real

import org.openmole.spatialdata.vector.SpatialField

import scala.util.Random

case class DiscreteCorrelatedSpatialFieldPerturbator(

                                                    ) extends SpatialFieldPerturbator {

  override def perturbateSpatialField(field: SpatialField)(implicit rng: Random): SpatialField = field

}
