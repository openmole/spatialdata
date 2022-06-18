package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.{DenseMatrix, Linear, Matrix}


/**
  * Simulation result for a macroscopic model run on a real system
  *
  * @param targetPopulation target population matrix
  * @param simulatedPopulation simulated population matrix
  * @param logmse log(MSE)
  * @param mselog MSE on log of populations
  */
case class MacroResultFit(
                   targetPopulation: Matrix,
                   simulatedPopulation: Matrix,
                   logmse: Double,
                   mselog: Double
                 ) extends MacroResult {
  override def toString: String = "logmse = "+logmse+" ; mselog = "+mselog
}




object MacroResultFit {


  def apply(targetPopulation: Matrix,simulatedPopulation: Matrix): MacroResultFit = MacroResultFit(
    targetPopulation,simulatedPopulation,
    Linear.logmse(simulatedPopulation,targetPopulation),
    Linear.mselog(simulatedPopulation,targetPopulation)
  )

  def apply(targetPopulation: Matrix, simulatedStates: Vector[MacroState]): MacroResultFit = {
    val res = DenseMatrix.zeros(targetPopulation.nrows,targetPopulation.ncols)
    simulatedStates.zip(0 until res.ncols).foreach{case (s,p) => res.setMSubmat(0,p,s.populations.values)}
    MacroResultFit(targetPopulation,res)
  }


}