package org.openmole.spatialdata.scalatest.model.spatialinteraction

import org.openmole.spatialdata.scalatest.model.spatialinteraction.synthetic.ModelSpatialInteractionSyntheticSuite
import org.scalatest.Suites

class ModelSpatialInteractionSuite extends Suites(
  new SinglyConstraintSpIntModelFlatSpec,
  new ModelSpatialInteractionSyntheticSuite
)

