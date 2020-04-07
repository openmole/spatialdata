package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.{Linear, Matrix}


/**
  * Simulation result for a macroscopic model
  * @param targetPopulation
  * @param simulatedPopulation
  * @param logmse
  * @param mselog
  */
case class MacroResult(
                   targetPopulation: Matrix,
                   simulatedPopulation: Matrix,
                   logmse: Double,
                   mselog: Double
                 ) {
  override def toString: String = "logmse = "+logmse+" ; mselog = "+mselog
}




object MacroResult {


  def apply(targetPopulation: Matrix,simulatedPopulation: Matrix): MacroResult = MacroResult(
    targetPopulation,simulatedPopulation,
    Linear.logmse(simulatedPopulation,targetPopulation),
    Linear.mselog(simulatedPopulation,targetPopulation)
  )

}