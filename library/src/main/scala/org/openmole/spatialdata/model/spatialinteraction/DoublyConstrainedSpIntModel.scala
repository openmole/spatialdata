package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{DenseMatrix, EmptyMatrix, Matrix, SparseMatrix}
import org.openmole.spatialdata.vector.SpatialField

object DoublyConstrainedSpIntModel {

  /**
    * Computes weights for doubly constrained flows
    *   Tij = A_i B_j O_i D_j c_{ij}
    * where A_i are such that \sum_j Tij = Oi and B_j such that \sum_i Tij = Dj
    *  - travel cost parameters are given, does not calibrate them
    *
    * @param originMasses
    * @param destinationMasses
    * @param costMatrix
    * @param tolerance
    * @return
    */
  def doublyConstrainedFlows(originMasses: Seq[Double],
                             destinationMasses: Seq[Double],
                             costMatrix: Matrix,
                             tolerance: Double = 0.01): Matrix = {
    // FIXME should add check on dimensions
    costMatrix match {
      case _: DenseMatrix => Matrix.MatrixImplementation.setDefaultDense
      case _: SparseMatrix => Matrix.MatrixImplementation.setDefaultSparse
      case _: EmptyMatrix => throw new UnsupportedOperationException("Cost matrix can not be empty")
    }

    val origin: Matrix = Matrix(originMasses.toArray,false)
    val destination: Matrix = Matrix(destinationMasses.toArray,false)
    val initialOriginWeights = Matrix(Array.fill(originMasses.size)(1.0 / originMasses.size.toDouble),false)
    val initialDestinationWeights = Matrix(Array.fill(destinationMasses.size)(1.0 / destinationMasses.size.toDouble),false)

    utils.log(s"cost matrix avg= ${costMatrix.values.flatten.sum/(costMatrix.nrows*costMatrix.ncols)}")
    utils.log(s"origin avg=${origin.values.flatten.sum/origin.values.flatten.length}")
    utils.log(s"dest avg=${destination.values.flatten.sum/origin.values.flatten.length}")
    /**
      * state composed by column vectors of weights and absolute weight variation (relative ?)
      * @param state
      * @return
      */
    def iterateWeights(state: (Matrix,Matrix,Double)): (Matrix,Matrix,Double) = {
      val prevOriginWeights = state._1
      val prevDestWeights = state._2
      //utils.log("Prev orig weights: "+prevOriginWeight+" ; Prev dest weights: "+prevDestWeights)
      //println(s"avg ow = ${prevOriginWeights.values.flatten.sum/prevOriginWeights.values.flatten.length}")
      //println(s"avg dw = ${prevDestWeights.values.flatten.sum/prevDestWeights.values.flatten.length}")
      val originWeightsUnnorm = (costMatrix %*% (prevDestWeights * origin)).map(1/_)
      val destWeightsUnnorm = (costMatrix %*% (prevOriginWeights * destination)).map(1/_)
      val (otot,dtot) = (originWeightsUnnorm.sum,destWeightsUnnorm.sum)
      val (originWeights,destWeights) = (originWeightsUnnorm.map(_ / otot),destWeightsUnnorm.map(_ / dtot))
      val epsilon = ((prevOriginWeights - originWeights).map(math.abs).values.flatten.sum/(prevOriginWeights.values.flatten.sum + originWeights.values.flatten.sum)) + ((prevDestWeights - destWeights).map(math.abs).values.flatten.sum/(prevDestWeights.values.flatten.sum + destWeights.values.flatten.sum))
      utils.log("Doubly constrained flows: epsilon = "+epsilon)
      (originWeights,destWeights,epsilon)
    }
    val (ow,dw,_) = Iterator.iterate((initialOriginWeights,initialDestinationWeights,Double.MaxValue.toDouble))(iterateWeights).takeWhile(_._3>tolerance).toSeq.last
    //Matrix.cbind(Array.fill(destination.nrows)(ow*origin))*Matrix.rbind(Array.fill(origin.nrows)((dw*destination).transpose))*costMatrix
    val ocol = (ow*origin).values.flatten
    val drow = (dw*destination).values.flatten
    // FIXME not efficient with sparse here ?
    Matrix(Array.fill(drow.length)(ocol).transpose)*Matrix(Array.fill(ocol.length)(drow))*costMatrix
  }


}
