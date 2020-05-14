package org.openmole.spatialdata.model.urbandynamics
import org.openmole.spatialdata.utils.math.Matrix

/**
  *
  * @param gibratGrowthRates city specific growth rate (endogenous self-sustained growth)
  */
case class Gibrat(
                   gibratGrowthRates: Vector[Double]
                 ) extends MacroGrowthRate {
  override def growthRate: Matrix => Matrix = m =>  Matrix(gibratGrowthRates.toArray,row=false)(Matrix.getImplementation(m))
}
