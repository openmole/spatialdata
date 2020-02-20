package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.model.spatialinteraction.{FittedSpIntModel, SinglyConstrainedSpIntModel, SpatialInteractionModel}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{DenseMatrix, EmptyMatrix, Matrix, SparseMatrix}
import org.openmole.spatialdata.vector.SpatialField


case class QUANTOneMode(
                //override val observedFlows: Matrix,
                //override val distances: Matrix,
                //override val originValues: SpatialField[Double],
                //override val destinationValues: SpatialField[Double],
                //override val predictedFlows: Matrix
                model: SinglyConstrainedSpIntModel
                ) extends SpatialInteractionModel with FittedSpIntModel  { // FIXME should extend SinglyConstr to have fittedParam property

  //override def fit: SpatialInteractionModel => SpatialInteractionModel = QUANTOneMode.fit

  override def fit: SpatialInteractionModel => SpatialInteractionModel = {
    model => model match {
      case quant: QUANTOneMode => this.copy(model=SinglyConstrainedSpIntModel.fitSinglyConstrainedSpIntModel(quant.model))
      case _ => throw new UnsupportedOperationException("Quant one mode is singly constraint")
    }
  }

  //def fitted: SpatialInteractionModel = fit(this)

  override def observedFlows: Matrix = model.observedFlows
  override def distances: Matrix = model.distances
  override def destinationValues: SpatialField[Double] = model.destinationValues
  override def originValues: SpatialField[Double] = model.originValues
  override def predictedFlows: Matrix = model.predictedFlows

}


object QUANTOneMode {

  /**
    * from processed files
    * @param sparseFlowsFile
    * @param dmatFile
    * @return
    */
  def apply(sparseFlowsFile: String, dmatFile: String): QUANTOneMode = {

    // FIXME set breeze default as used anyway and readSparseFromDense gives a Breeze
    SparseMatrix.SparseMatrixImplementation.setImplSparseBreeze

    //val dmatdense = DenseMatrix(CSV.readMat(dmatFile))
    val dmat = CSV.readSparseMatFromDense(dmatFile, {d=> math.exp( - d / 60.0) > 0.3}) // FIXME in the end should do a filtering on OiDjcij
    utils.log(s"sparse dmat: $dmat")
    val flowmat = CSV.readSparseMat(sparseFlowsFile)
    utils.log(s"sparse flowmat: $flowmat")
    // note: at this stage, no need of coordinates and spatial fields, just O/D values indeed
    //println(s"flowmat: ${flowmat.nrows}x${flowmat.ncols}")
    val originVals: Array[Double] = flowmat.rowSum
    //println(originVals.toSeq) //!  same keys in map: spatial field has unique value for each point
    val origin: SpatialField[Double]=originVals.zipWithIndex.map{case (s,i) => ((i.toDouble,0.0),Array(s))}.toMap
    //println(origin)
    val destination = flowmat.colSum.zipWithIndex.map{case (s,j) => ((j.toDouble,0.0),Array(s))}.toMap

    QUANTOneMode(model = SinglyConstrainedSpIntModel(flowmat,dmat,origin,destination))
  }


}
