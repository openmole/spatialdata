package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.Matrix

/**
  * Transforms a population column matrix into a growth rate column matrix, used in macro model with additive growth process
  */
trait MacroGrowthRate {
  def growthRate: Matrix => Matrix
}
