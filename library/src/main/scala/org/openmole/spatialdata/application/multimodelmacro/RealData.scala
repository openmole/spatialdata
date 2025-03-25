package org.openmole.spatialdata.application.multimodelmacro

import org.openmole.spatialdata.model.urbandynamics.{Coevolution, EconomicExchanges, Innovation, MacroResultFit, MultiMacroModel}
import org.openmole.spatialdata.model.urbandynamics.MultiMacroModel.MultiMacroResult
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
    def logmse: Double = 0.0
    def mselog: Double = 0.0
  }

  object Result {
    def apply(result: MultiMacroResult): Result = {
      val targetPop = result.states.head.populations
      val simulatedStates = result.states.toVector
      val fit = MacroResultFit(targetPop, simulatedStates)
      Result(result, fit)
    }
  }

  def setupRealData(populationsFile: String,
                    distancesFile: String,
                    datesFile: String):  (Matrix, Matrix, Array[Double]) = {
    implicit val m: MatrixImplementation = Matrix.defaultImplementation
    rng.setSeed(seed)

    // load populations, dates, distance matrix
    val populationMatrix: Matrix = Matrix(CSV.readMat(populationsFile))
    val distancesMatrix: Matrix = distancesFile.length match { // no calib on effective distances (pop only for now) -> only initial distance
      case n if n > 0 => Matrix(CSV.readMat(distancesFile))
      case _ => DenseMatrix.zeros(populationMatrix.nrows, populationMatrix.ncols)
    }
    val rawdates: Seq[String] = CSV.readCSV(datesFile, withHeader = false).values.toSeq.head
    val dates: Array[Double] = rawdates.map(_.toDouble).toArray

    (populationMatrix, distancesMatrix, dates)
  }

}
