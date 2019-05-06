package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

/**
  * matrix utilities (not linear algebra operations which are in [[Linear]]
  */
object Matrix {


  def fromVector(vector: Vector[Vector[Double]]): RealMatrix = MatrixUtils.createRealMatrix(vector.map{_.toArray}.toArray)

  def fromColumnVector(vector: Vector[Double]): RealMatrix = MatrixUtils.createColumnRealMatrix(vector.toArray)

  def toFlatVector(realMatrix: RealMatrix): Vector[Double] = realMatrix.getData.flatten.toVector

  def fromArray(a: Array[Array[Double]]): RealMatrix = MatrixUtils.createRealMatrix(a)



}
