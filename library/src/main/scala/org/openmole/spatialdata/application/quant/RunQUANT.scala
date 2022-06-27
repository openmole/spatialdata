package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.application.quant.QUANT._
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.{Binary, CSV}
import org.openmole.spatialdata.utils.math.SparseMatrix

/**
  * To investigate:
  *   - test different dmat sparsity param values + sparsity on actual flows ?
  *   - runtime/perfs (compare parallelisation with sparse mat implementation)
  */
object RunQUANT extends App {

  implicit val sparseMatrixImplementation: SparseMatrix.SparseMatrixImplementation = SparseMatrix.SparseBreeze()

  args.length match {
    case 0 => println("Usage: --onemode | --multimode | --convert-data")
    case 1 =>
      args(0) match {
        case "--onemode" => calibrateQUANTOneMode()
        case "--multimode" => calibrateQUANTMultiMode()
        case "--convert-data" => convertData()
      }
  }



}
