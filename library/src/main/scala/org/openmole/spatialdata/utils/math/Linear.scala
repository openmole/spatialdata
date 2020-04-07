package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.linear.SingularMatrixException


/**
  * Operations on/with matrices
  */
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
      val matrix: RealMatrix = RealMatrix(M)
      //should ensure the matrix is invertible by perturbating it if necessary
      //implicit val (m,dn,sm) = Matrix.defaultImplementations
      val res: Matrix = matrix.inverse%*%RealMatrix(B,row=false)
      Some(res.flatValues)
    } catch {
      // FIXME should be able to use any matrix impl
      case _: SingularMatrixException => None
    }
  }

  /**
    * MSE on logs of populations
    *
    * @param m
    * @param target
    * @return
    */
  def mselog(m: Matrix,target: Matrix): Double = {
    val logres: Matrix = if (m.ncols==target.ncols&m.nrows==target.nrows)
       m.map { d => Math.log(d) }
    else DenseMatrix.zeros(target.nrows,target.ncols)
    val logreal = target.map { d => Math.log(d) }
    val sqdiff = (logres - logreal).map{ d => d*d}
    sqdiff.sum / (sqdiff.nrows*sqdiff.ncols)
  }

  /**
    * Log of MSE
    *
    * @param m
    * @param target
    * @return
    */
  def logmse(m: Matrix,target: Matrix): Double = {
    val sqdiff = if(m.ncols==target.ncols&m.nrows==target.nrows)
      (m - target).map{d => d*d} else {
      val zeros = DenseMatrix.zeros(target.nrows,target.ncols)
      (zeros - target).map{d => d*d}
    }
    Math.log(sqdiff.sum)
  }



}
