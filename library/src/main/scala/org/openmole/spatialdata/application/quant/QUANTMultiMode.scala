package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.model.spatialinteraction.{SinglyConstrainedMultiModeSpIntModel, SinglyConstrainedSpIntModel}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.SparseMatrix
import org.openmole.spatialdata.vector.SpatialField

object QUANTMultiMode {


  /**
    * Multi mode QUANT from files
    *
    *  ! in the end should do a filtering on OiDjcij when sparsing the matrix
    *
    * @param sparseFlows sparse flow matrices
    * @param sparseDistanceWeights sparse distance matrices
    *
    * @return
    */
  def QUANTMultiMode(sparseFlows: Array[SparseMatrix],
                     sparseDistanceWeights: Array[SparseMatrix]
                    )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SinglyConstrainedMultiModeSpIntModel = {

    // needs to sum all modes
    // rq: change in 2.13: reduce needs an explicit function - pb expansion?
    def asum: (Array[Double],Array[Double]) => Array[Double] = {case a: (Array[Double],Array[Double]) => a._1.zip(a._2).map{case (o1,o2) => o1+o2}}
    val originVals: Array[Double] = sparseFlows.map(_.rowSum).reduce(asum)
    utils.log(s"origin values length = ${originVals.length}")
    val origin: SpatialField[Double]=originVals.zipWithIndex.map{case (s,i) => ((i.toDouble,0.0),Array(s))}.toMap
    val destination = sparseFlows.map(_.colSum).reduce(asum).zipWithIndex.map{case (s,j) => ((j.toDouble,0.0),Array(s))}.toMap
    SinglyConstrainedMultiModeSpIntModel(
      sparseFlows.zip(sparseDistanceWeights).map{case (flowmat,wmat) =>
        SinglyConstrainedSpIntModel(
          observedFlows = flowmat,
          distanceWeightsMatrix = Some(wmat),
          originValues = origin,
          destinationValues = destination
        )
      }
    )
  }

}
