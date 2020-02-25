package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.SparseMatrix

import scala.util.Random

trait FlowsGenerator {
  /**
    * Flows imply O/D and distance matrix: they are a spatial interaction model without prediction in our setting
    * @param rng
    * @return
    */
  def generateFlows(implicit rng: Random, spMatImpl: SparseMatrix.SparseMatrixImplementation): SpatialInteractionModel
}
