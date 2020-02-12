package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.linear



sealed trait Matrix

/**
  * matrix utilities (not linear algebra operations which are in [[Linear]]
  *
  * TODO test spark matrices https://spark.apache.org/docs/2.1.2/api/java/org/apache/spark/mllib/linalg/Matrix.html
  *
  */
object Matrix {


  def fromVector(vector: Vector[Vector[Double]]): RealMatrix = RealMatrix(linear.MatrixUtils.createRealMatrix(vector.map{_.toArray}.toArray))

  def fromColumnVector(vector: Vector[Double]): RealMatrix = fromColumnArray(vector.toArray)

  def toFlatVector(realMatrix: linear.RealMatrix): Vector[Double] = realMatrix.getData.flatten.toVector

  def fromArray(a: Array[Array[Double]]): RealMatrix = RealMatrix(linear.MatrixUtils.createRealMatrix(a))

  def fromColumnArray(a: Array[Double]): RealMatrix = RealMatrix(linear.MatrixUtils.createColumnRealMatrix(a))

  def toFlatArray(realMatrix: linear.RealMatrix): Array[Double] = realMatrix.getData.flatten

}

case class RealMatrix(m: linear.RealMatrix) extends Matrix

object RealMatrix {

}


/**
  * TODO breeze DenseMatrix
  *  and / or spark
  */
case class DenseMatrix() extends Matrix

object DenseMatrix {
  val empty = DenseMatrix()
}


/**
  * although not super efficient before matrix operations, string indices can be used foe correspondance between dimensions
  * @param m
  * @param rowNames
  * @param colNames
  */
case class IndexedMatrix(
                        m: Matrix,
                        rowNames: Seq[String],
                        colNames: Seq[String]
                        )



