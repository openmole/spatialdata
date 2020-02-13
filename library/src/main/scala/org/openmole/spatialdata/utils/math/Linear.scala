package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.linear._

object Linear {

  /**
    * Solve a matrix system if the matrix is not singular
    * @param M
    * @param B
    * @return
    */
  def solveSystem(M: Array[Array[Double]],B: Array[Double]): Option[Array[Double]] = {
    try{
      // convert the objects to math3 objects
      val matrix: RealMatrix = MatrixUtils.createRealMatrix(M)
      //should ensure the matrix is invertible by perturbating it if necessary
      val inv = MatrixUtils.inverse(matrix)
      val res: RealVector = inv.operate(MatrixUtils.createRealVector(B))
      Some(res.toArray)
    } catch {
      case e: SingularMatrixException => None
    }
  }

}
