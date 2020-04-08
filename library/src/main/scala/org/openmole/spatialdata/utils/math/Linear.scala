package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.linear.SingularMatrixException

import scala.util.Random


/**
  * Operations on/with matrices
  */
object Linear {

  /**
    * Solve a matrix system if the matrix is not singular
    *
    *  !! should be able to use any matrix impl: inverse defined everywhere?
    *
    * @param M system matrix
    * @param B constant
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
      case _: SingularMatrixException => None
    }
  }

  /**
    * MSE on logs of populations
    *
    * @param m simulated
    * @param target target
    * @return
    */
  def mselog(m: Matrix, target: Matrix): Double = {
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
    * @param m simulated
    * @param target target
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


  /**
    * random distance matrix
    * @param n number of points
    * @param worldSize world width
    * @param rng random
    * @return
    */
  def randomDistanceMatrix(n: Int,worldSize: Double)(implicit rng: Random): Matrix= {
    val xcoords = Vector.fill(n)(rng.nextDouble()*worldSize)
    val ycoords = Vector.fill(n)(rng.nextDouble()*worldSize)

    val flatres = for {
      x1 <- xcoords.zip(ycoords)
      x2 <- xcoords.zip(ycoords)
    } yield math.sqrt(math.pow(x1._1-x2._1,2)+math.pow(x1._2-x2._2,2))

    Matrix(flatres.toArray.sliding(n,n).toArray)(Matrix.defaultImplementation)
  }



}
