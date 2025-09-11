package org.openmole.spatialdata.application.multimodelmacro

import org.openmole.spatialdata.model.urbandynamics.{Coevolution, EconomicExchanges, Innovation, MacroResultFit, MultiMacroModel}
import org.openmole.spatialdata.model.urbandynamics.MultiMacroModel.{MultiMacroResult, MultiMacroState}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{DenseMatrix, EmptyMatrix, Matrix, Statistics}
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.vector.measures.Spatstat
import org.openmole.spatialdata.vector.synthetic.RandomPointsGenerator

import scala.util.Random

/**
  * Run the model on real data (calibration, PSE)
  */
case class RealData()

object RealData {

  case class Result(
                     result: MultiMacroResult,
                     fit: MacroResultFit
                   ){
    def logmse: Double = fit.logmse
    def mselog: Double = fit.mselog
  }

  object Result {
    def apply(result: MultiMacroResult): Result = {
      val targetPop = result.realPopulations
      val simulatedStates: Vector[MultiMacroState] = result.states.toVector
      utils.log(s"Computing real fit: total target pops ${targetPop.colSum.toSeq}; simulated pops: ${simulatedStates.map(_.populations.sum)}")
      val fit = MacroResultFit(targetPop, simulatedStates)
      Result(result, fit)
    }
  }

  def setupRealData(populationsFile: String,
                    distancesFile: String,
                    datesFile: String):  (Matrix, Matrix, Array[Double]) = {
    implicit val m: MatrixImplementation = Matrix.defaultImplementation

    // load populations, dates, distance matrix
    val populationMatrix: Matrix = Matrix(CSV.readMat(populationsFile))
    val distancesMatrix: Matrix = distancesFile.length match { // no calib on effective distances (pop only for now) -> only initial distance
      case n if n > 0 => Matrix(CSV.readMat(distancesFile))
      case _ => DenseMatrix.zeros(populationMatrix.nrows, populationMatrix.ncols)
    }
    val rawdates: Seq[String] = CSV.readCSV(datesFile, withHeader = false).values.toSeq.head
    val dates: Array[Double] = rawdates.map(_.toDouble).toArray

    utils.log(s"Real data setup : total populations ${populationMatrix.colSum.toSeq} ; average distance ${distancesMatrix.flatValues.sum/(distancesMatrix.nrows*distancesMatrix.nrows)}")

    (populationMatrix, distancesMatrix, dates)
  }

}
