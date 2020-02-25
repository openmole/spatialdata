package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.model.spatialinteraction.{SinglyConstrainedMultiModeSpIntModel, SinglyConstrainedSpIntModel}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.SparseMatrix
import org.openmole.spatialdata.vector.SpatialField

object QUANTMultiMode {


  /**
    * multi mode QUANT from files
    * @param sparseFlowsFiles
    * @param dmatFiles
    * @return
    */
  def quantMultiMode(sparseFlowsFiles: Array[String], dmatFiles: Array[String]): SinglyConstrainedMultiModeSpIntModel = {
    SparseMatrix.SparseMatrixImplementation.setImplSparseBreeze
    val dmats = dmatFiles.map(dmatFile => CSV.readSparseMatFromDense(dmatFile, {d=> math.exp( - d / 60.0) > 0.3})) // FIXME in the end should do a filtering on OiDjcij
    utils.log(s"sparse dmat: ${dmats.toSeq}")
    val flowmats = sparseFlowsFiles.map(sparseFlowsFile => CSV.readSparseMat(sparseFlowsFile))
    utils.log(s"sparse flowmat: ${flowmats.toSeq}")
    // needs to sum all modes
    // FIXME change in 2.13: reduce needs an explicit function - pb expansion?
    def asum: (Array[Double],Array[Double]) => Array[Double] = {case a: (Array[Double],Array[Double]) => a._1.zip(a._2).map{case (o1,o2) => o1+o2}}
    val originVals: Array[Double] = flowmats.map(_.rowSum).reduce(asum)
    utils.log(s"origin values length = ${originVals.length}")
    val origin: SpatialField[Double]=originVals.zipWithIndex.map{case (s,i) => ((i.toDouble,0.0),Array(s))}.toMap
    val destination = flowmats.map(_.colSum).reduce(asum).zipWithIndex.map{case (s,j) => ((j.toDouble,0.0),Array(s))}.toMap
    SinglyConstrainedMultiModeSpIntModel(
      flowmats.zip(dmats).map{case (flowmat,dmat) =>
        SinglyConstrainedSpIntModel(flowmat,dmat,origin,destination)
      }
    )
  }

}
