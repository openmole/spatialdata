package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.model.spatialinteraction.SinglyConstrainedSpIntModel
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{EmptyMatrix, SparseMatrix}
import org.openmole.spatialdata.vector.SpatialField


/*
//  this is not necessary as introduces unnecessary redundancy, as long as only wraps a model (useful to have the QUANTOneMode type though)
case class QUANTOneMode(
                //override val observedFlows: Matrix,
                //override val distances: Matrix,
                //override val originValues: SpatialField[Double],
                //override val destinationValues: SpatialField[Double],
                //override val predictedFlows: Matrix
                model: SinglyConstrainedSpIntModel
                ) extends SpatialInteractionModel with FittedSpIntModel  { //  should extend SinglyConstr to have fittedParam property

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
*/


object QUANTOneMode {

  /**
    * from processed files
    *
    *  ! in the end should do a filtering on OiDjcij for sparse Matrices instead of separately observed flows/distance weights
    *
    * @param sparseFlows sparse flows matrix
    * @param sparseDistanceWeights distance weights matrix (exp - d_ij / d_0)
    * @return
    */
  def QUANTOneMode(
                    sparseFlows: SparseMatrix,
                    sparseDistanceWeights: SparseMatrix
                  )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SinglyConstrainedSpIntModel = {
    // note: at this stage, no need of coordinates and spatial fields, just O/D values indeed
    //println(s"flowmat: ${flowmat.nrows}x${flowmat.ncols}")
    val originVals: Array[Double] = sparseFlows.rowSum
    //println(originVals.toSeq) //!  same keys in map: spatial field has unique value for each point
    val origin: SpatialField[Double]=originVals.zipWithIndex.map{case (s,i) => ((i.toDouble,0.0),Array(s))}.toMap
    //println(origin)
    val destination = sparseFlows.colSum.zipWithIndex.map{case (s,j) => ((j.toDouble,0.0),Array(s))}.toMap

    SinglyConstrainedSpIntModel(
      observedFlows = sparseFlows,
      distanceWeightsMatrix = Some(sparseDistanceWeights),
      originValues = origin,
      destinationValues = destination
    )
  }


}
