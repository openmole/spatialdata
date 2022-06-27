package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.model.spatialinteraction.{SinglyConstrainedMultiModeMultiClassSpIntModel, SinglyConstrainedMultiModeSpIntModel, SinglyConstrainedSpIntModel}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.{Binary, CSV}
import org.openmole.spatialdata.utils.math.{Matrix, SparseMatrix}
import org.openmole.spatialdata.vector.SpatialField

object QUANTMultiModeMultiClass {


  def quantMultiModeMultiClass(
                                sparseModeFlowsFiles: Array[String],
                                sparseClassFlowsFiles: Array[String],
                                dmatFiles: Array[String],
                                csvInput: Boolean = false
                    )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SinglyConstrainedMultiModeMultiClassSpIntModel = {

    val dmats = if (csvInput) dmatFiles.map(dmatFile =>
      CSV.readSparseMatFromDense(
        dmatFile,
        {d=> math.exp( - d / 60.0) > 0.3},
        {d=> math.exp( - d / 60.0)}
      )
    )
    else dmatFiles.map(Binary.readBinary[SparseMatrix])
    utils.log(s"sparse dmat: ${dmats.toSeq}")

    val modeflowmats = if (csvInput) sparseModeFlowsFiles.map(sparseFlowsFile => CSV.readSparseMat(sparseFlowsFile))
    else sparseModeFlowsFiles.map(Binary.readBinary[SparseMatrix])
    utils.log(s"sparse modeflowmat: ${modeflowmats.toSeq}")

    val classflowmats = if (csvInput) sparseClassFlowsFiles.map(sparseFlowsFile => CSV.readSparseMat(sparseFlowsFile))
    else sparseClassFlowsFiles.map(Binary.readBinary[SparseMatrix])
    utils.log(s"sparse classflowmat: ${modeflowmats.toSeq}")

    def asum: (Array[Double],Array[Double]) => Array[Double] = {case a: (Array[Double],Array[Double]) => a._1.zip(a._2).map{case (o1,o2) => o1+o2}}
    val originVals: Array[Double] = modeflowmats.map(_.rowSum).reduce(asum)
    utils.log(s"origin values length = ${originVals.length}")
    val origin: SpatialField[Double]=originVals.zipWithIndex.map{case (s,i) => ((i.toDouble,0.0),Array(s))}.toMap
    val destination = modeflowmats.map(_.colSum).reduce(asum).zipWithIndex.map{case (s,j) => ((j.toDouble,0.0),Array(s))}.toMap

    SinglyConstrainedMultiModeMultiClassSpIntModel(
      modeflowmats.asInstanceOf[Array[Matrix]],
      dmats.asInstanceOf[Array[Matrix]],
      classflowmats.asInstanceOf[Array[Matrix]],
      origin,
      destination
    )
  }


}
