package org.openmole.spatialdata.model.spatialinteraction

import scala.util.Random

trait FlowsGenerator {
  /**
    * Flows imply O/D and distance matrix: they are a spatial interaction model without prediction in our setting
    * @param rng
    * @return
    */
  def generateFlows(implicit rng: Random): SpatialInteractionModel
}
