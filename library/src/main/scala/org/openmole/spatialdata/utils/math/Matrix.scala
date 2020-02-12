package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.linear
import org.apache.commons.math3.linear.LUDecomposition


/**
  * FIXME could make generic?
  */
sealed trait Matrix {

  // rq: in immutable terms, setting is inefficient (should clone the underlying matrix?)
  def set(i: Int, j: Int, v: Double): Matrix
  def get(i: Int, j: Int): Double

  def map(f: Double => Double): Matrix

  // !! mutable
  def values: Array[Array[Double]]

  //basic ring operations
  def +(m: Matrix): Matrix
  def *(m: Matrix): Matrix

  def transpose: Matrix
  //def inverse: Matrix // should use pseudo inverse or restrict?
  def determinant: Double

}

/**
  * matrix utilities (not linear algebra operations which are in [[Linear]]
  *
  * TODO test spark matrices https://spark.apache.org/docs/2.1.2/api/java/org/apache/spark/mllib/linalg/Matrix.html
  *
  */
object Matrix {

  sealed trait MatrixImplementation
  case class Dense() extends MatrixImplementation
  case class Sparse() extends MatrixImplementation
  // dirty but avoids passing a context as implicit everywhere
  private var DEFAULT_MATRIX_IMPLEMENTATION: MatrixImplementation = Dense()

  def setDefaultDense: Unit = DEFAULT_MATRIX_IMPLEMENTATION = Dense()
  def setDefaultSparse: Unit = DEFAULT_MATRIX_IMPLEMENTATION = Sparse()


  /**
    * Use default implementation
    *
    * FIXME if alternative implementations, find a way to switch within Dense / Sparse
    *
    * @param a
    * @return
    */
  def apply(a: Array[Array[Double]]): Matrix = DEFAULT_MATRIX_IMPLEMENTATION match {
    case _: Dense => RealMatrix(a)
    case _: Sparse => SparseMatrixImpl(a)
  }

  //def apply(vector: Vector[Vector[Double]]): Matrix = apply(vector.map{_.toArray}.toArray)

  def apply(a: Array[Double], row: Boolean=true): Matrix = if(row) apply(Array(a)) else apply(a.map(Array(_)))

  //def apply(a: Vector[Double], row: Boolean=true): Matrix = apply(a.toArray,row)

  // FIXME get data methods should go into specific impl ?
  //def toFlatArray(realMatrix: linear.RealMatrix): Array[Double] = realMatrix.getData.flatten

}


/**
  * Dense matrix
  */
sealed trait DenseMatrix extends Matrix

/**
  * Apache commons real matrix as implementation of dense matrix
  * @param m
  */
case class RealMatrix(m: linear.RealMatrix) extends DenseMatrix {

  // the object clone does not clone internal references ?
  override def clone: RealMatrix = RealMatrix(m.getData.clone.map(_.clone))

  override def set(i: Int, j: Int, v: Double): Matrix = {
    val d = clone.m
    d.setEntry(i,j,v)
    RealMatrix(d)
  }

  /**
    * Mutable set
    * @param i
    * @param j
    * @param v
    */
  def setM(i: Int, j: Int, v: Double): Unit = m.setEntry(i,j,v)

  override def get(i: Int, j: Int): Double = m.getEntry(i,j)

  /**
    * for map, go through each element anyway, cloning is less an issue
    * @param f
    * @return
    */
  override def map(f: Double => Double): Matrix = {
    val d = m.getData.clone.map(_.clone)
    RealMatrix(d.map(_.map(f)))
  }

  override def values: Array[Array[Double]] = m.getData


  def dispatchOp(op: RealMatrix => RealMatrix): Matrix => Matrix = {
    m2: Matrix => m2 match {
      case m2: RealMatrix => op(m2)
      case _ => throw new UnsupportedOperationException("Matrix implementations combination not supported")
    }
  }

  override def +(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m2.m.add(m))}(m2)
  override def *(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m2.m.multiply(m))}(m2)
  override def transpose: Matrix = RealMatrix(m.transpose())
  override def determinant: Double = new LUDecomposition(m).getDeterminant

}

object RealMatrix {

  def apply(a: Array[Array[Double]]): RealMatrix = RealMatrix(linear.MatrixUtils.createRealMatrix(a))

}

/**
  * TODO breeze DenseMatrix
  *  and / or spark ?
  */
//case class DenseMatrix() extends Matrix

//object DenseMatrix {
//  val empty = DenseMatrix()
//}



sealed trait SparseMatrix extends Matrix

case class SparseMatrixImpl(m: linear.OpenMapRealMatrix) extends Matrix {

  // FIXME this is highly inefficient: for setting one element, the immutable constraint leads to cloning everything!
  // TODO alternative impl and some tests? or switch to mutable :/
  override def clone: SparseMatrixImpl = SparseMatrixImpl(new linear.OpenMapRealMatrix(m))

  override def set(i: Int,j: Int, v: Double): Matrix = {
    val d = this.clone.m
    d.setEntry(i,j,v)
    SparseMatrixImpl(d)
  }

  def setM(i: Int, j: Int, v: Double): Unit = m.setEntry(i,j,v)

  override def get(i: Int, j: Int): Double = m.getEntry(i,j)

  /**
    * FIXME does not exploit the sparse structure - should keep the index of non zero elements to iterate only on these?
    *  -> maybe reimplement with proper map would be more straightforward
    * @param f
    * @return
    */
  override def map(f: Double => Double): Matrix = {
    val d = m.getData.clone.map(_.clone)
    SparseMatrixImpl(d.map(_.map(f)))
  }

  override def values: Array[Array[Double]] = m.getData


  def dispatchOp(op: SparseMatrixImpl => SparseMatrixImpl): Matrix => Matrix = {
    m2: Matrix => m2 match {
      case m2: SparseMatrixImpl => op(m2)
      case _ => throw new UnsupportedOperationException("Matrix implementations combination not supported")
    }
  }

  override def +(m2: Matrix): Matrix = dispatchOp {m2 => SparseMatrixImpl(m2.m.add(m))}(m2)
  override def *(m2: Matrix): Matrix = dispatchOp {m2 => SparseMatrixImpl(m2.m.multiply(m))}(m2)
  // ! transpose not implemented -> transforms into a real matrix
  // cannot go through sparse mat entries: shitty implementation
  override def transpose: Matrix = RealMatrix(m.transpose())
  override def determinant: Double = new linear.LUDecomposition(m).getDeterminant

}

object SparseMatrixImpl {
  def apply(a: Array[Array[Double]]): SparseMatrixImpl = {
    val (n,p) = (a.length,a(0).length) //assume the array is actually a matrix (same row size)
    val m:linear.OpenMapRealMatrix = new linear.OpenMapRealMatrix(n,p) // mutable !
    a.zipWithIndex.foreach{case (row,i) => row.zipWithIndex.foreach{case (v,j) => m.setEntry(i,j,v)}}
    SparseMatrixImpl(m)
  }
}






/**
  * although not super efficient before matrix operations, string indices can be used for correspondance between dimensions
  * @param m
  * @param rowNames
  * @param colNames
  */
case class IndexedMatrix(
                        m: Matrix,
                        rowNames: Seq[String],
                        colNames: Seq[String]
                        )



