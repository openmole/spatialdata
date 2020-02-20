package org.openmole.spatialdata.utils.math

import java.util

import org.apache.commons.math3.linear
import org.apache.commons.math3.linear.LUDecomposition
import breeze.linalg
import breeze.linalg.CSCMatrix
import org.openmole.spatialdata.utils.math.Matrix.Sparse

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * FIXME could make generic?
  */
sealed trait Matrix {

  /**
    * rq: in immutable terms, setting is inefficient (should clone the underlying matrix?)
    * @param i
    * @param j
    * @param v
    * @return
    */
  def set(i: Int, j: Int, v: Double): Matrix

  /**
    * get element
    * @param i
    * @param j
    * @return
    */
  def get(i: Int, j: Int): Double

  /**
    * map element by element
    * @param f
    * @return
    */
  def map(f: Double => Double): Matrix

  // !! mutable
  def values: Array[Array[Double]]

  def flatValues: Array[Double]

  def nrows: Int
  def ncols: Int

  //basic ring operations (R style for the notation)
  //def %+%(m: Matrix): Matrix // does not make any sense!
  def %*%(m: Matrix): Matrix

  // scalar operations
  def +(d: Double): Matrix = map(_+d)
  def -(d: Double): Matrix = map(_-d)
  def *(d: Double): Matrix = map(_*d)

  def +(m: Matrix): Matrix
  def -(m: Matrix): Matrix
  def *(m: Matrix): Matrix

  def transpose: Matrix
  //def inverse: Matrix // should use pseudo inverse or restrict?
  def determinant: Double

  def sum: Double
  def mean: Double = sum/(nrows*ncols)
  def min: Double
  def max: Double

  def rowSum: Array[Double]
  def colSum: Array[Double]

}

/**
  * matrix utilities (not linear algebra operations which are in [[Linear]]
  *
  * TODO test spark matrices https://spark.apache.org/docs/2.1.2/api/java/org/apache/spark/mllib/linalg/Matrix.html
  *
  * mutable / immutable matrices?
  * https://medium.com/@eob/how-you-might-create-a-scala-matrix-library-in-a-functional-programming-style-760f8bf6ee6
  *
  */
object Matrix {

  sealed trait MatrixImplementation
  case class Dense() extends MatrixImplementation
  case class Sparse() extends MatrixImplementation
  // dirty but avoids passing a context as implicit everywhere
  private var DEFAULT_MATRIX_IMPLEMENTATION: MatrixImplementation = Dense()
  object MatrixImplementation {
    def setDefaultDense: Unit = DEFAULT_MATRIX_IMPLEMENTATION = Dense()
    def setDefaultSparse: Unit = DEFAULT_MATRIX_IMPLEMENTATION = Sparse()
  }


  /**
    * Use default implementation
    *
    * FIXME if alternative implementations, find a way to switch within Dense / Sparse
    *
    * @param a
    * @return
    */
  def apply(a: Array[Array[Double]]): Matrix = DEFAULT_MATRIX_IMPLEMENTATION match {
    case _: Dense => DenseMatrix(a)
    case _: Sparse => SparseMatrix(a)
  }

  //def apply(vector: Vector[Vector[Double]]): Matrix = apply(vector.map{_.toArray}.toArray)

  def apply(a: Array[Double], row: Boolean=true): Matrix = if(row) apply(Array(a)) else apply(a.map(Array(_)))

  //def apply(a: Vector[Double], row: Boolean=true): Matrix = apply(a.toArray,row)

  // FIXME get data methods should go into specific impl ?
  //def toFlatArray(realMatrix: linear.RealMatrix): Array[Double] = realMatrix.getData.flatten

  /**
    * Row bind two matrices
    * FIXME rewrite specific for sparse mats
    *
    * FIXME need to test these
    *
    * @param a1
    * @param a2
    * @return
    */
  //def rbind(a1: Matrix, a2: Matrix): Matrix = Matrix(a1.values++a2.values)

  /**
    * Row bind several matrices
    * @param a
    * @return
    */
  //def rbind(a: Array[Matrix]): Matrix = a.reduce(rbind)

  /**
    * Column bind two matrices
    * @param a1
    * @param a2
    * @return
    */
  //def cbind(a1: Matrix, a2: Matrix): Matrix = Matrix((a1.values.transpose++a2.values.transpose).transpose)

  /**
    * Column bind several matrices
    * @param a
    * @return
    */
  //def cbind(a: Array[Matrix]): Matrix = a.reduce(cbind)


}


case class EmptyMatrix() extends Matrix {
  override def nrows: Int = 0
  override def ncols: Int = 0
  override def values: Array[Array[Double]] = Array.empty[Array[Double]]
  override def flatValues: Array[Double] = Array.empty[Double]
  override def get(i: Int, j: Int): Double = Double.NaN
  override def set(i: Int, j: Int, v: Double): Matrix = this
  override def %*%(m: Matrix): Matrix = this
  //override def %+%(m: Matrix): Matrix = this
  override def *(m: Matrix): Matrix = this
  override def +(m: Matrix): Matrix = this
  override def -(m: Matrix): Matrix = this
  override def map(f: Double => Double): Matrix = this
  override def determinant: Double = Double.NaN
  override def transpose: Matrix = this
  override def sum: Double = Double.NaN
  override def min: Double = Double.NaN
  override def max: Double = Double.NaN
  override def rowSum: Array[Double] = Array.empty[Double]
  override def colSum: Array[Double] = Array.empty[Double]
}

/**
  * Dense matrix
  */
sealed trait DenseMatrix extends Matrix {
  override def rowSum: Array[Double] = values.map{_.sum}
  override def colSum: Array[Double] = transpose.values.map{_.sum}
  override def sum: Double = values.flatten.sum
  override def min: Double = values.flatten.min(Ordering.Double.TotalOrdering)
  override def max: Double = values.flatten.max(Ordering.Double.TotalOrdering)
  override def flatValues: Array[Double] = values.flatten
}

object DenseMatrix {

  sealed trait DenseMatrixImplementation
  case class Real() extends DenseMatrixImplementation
  case class DenseBreeze() extends DenseMatrixImplementation
  private var DEFAULT_DENSE_MATRIX_IMPLEMENTATION: DenseMatrixImplementation = Real()
  object DenseMatrixImplementation {
    def setImplReal: Unit = DEFAULT_DENSE_MATRIX_IMPLEMENTATION = Real()
    def setImplDenseBreeze: Unit = DEFAULT_DENSE_MATRIX_IMPLEMENTATION = DenseBreeze()
  }

  def apply(a: Array[Array[Double]]): DenseMatrix = DEFAULT_DENSE_MATRIX_IMPLEMENTATION match {
    case _: Real => RealMatrix(a)
    case _: DenseBreeze => BreezeDenseMatrix(a)
    case _ => RealMatrix(a)
  }

  /**
    * zeros
    * @param n
    * @param p
    * @return
    */
  def zeros(n: Int, p: Int): DenseMatrix = constant(n,p,0.0)

  def ones(n: Int, p: Int): DenseMatrix = constant(n,p,1.0)

  def constant(n: Int, p: Int, v: Double): DenseMatrix = DenseMatrix(Array.fill(n)(Array.fill(p)(v)))

  /**
    * random dense matrix
    * @param n
    * @param p
    * @param density
    * @param rng
    * @return
    */
  def randomDenseMatrix(n: Int, p: Int, density: Double)(implicit rng: Random): DenseMatrix = {
    val m = zeros(n,p)
    //val inds: Seq[(Int,Int)] = Stochastic.sampleWithoutReplacement[(Int,Int)](for {i <- 0 until n;j <- 0 until p} yield (i,j), (n*p*density).toInt)
    //inds.map{case (i,j) => (i,j,rng.nextDouble)}
    // approximate density by drawing at each step
    val values = (for {i <- 0 until n
         j <- 0 until p
         r = rng.nextDouble()
    } yield  if (r < density) 0.0 else rng.nextDouble()).toArray.grouped(p).toArray
    DenseMatrix(values)
  }


}

/**
  * Apache commons real matrix as implementation of dense matrix
  * @param m
  */
case class RealMatrix(m: linear.RealMatrix) extends DenseMatrix {

  // the object clone does not clone internal references ?
  override def clone: RealMatrix = RealMatrix(m.getData.clone.map(_.clone))

  override def nrows: Int = m.getRowDimension
  override def ncols: Int = m.getColumnDimension

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

  //override def %+%(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.add(m2.m))}(m2)

  override def %*%(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.multiply(m2.m))}(m2)
  override def +(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 + v2}})}(m2)
  override def -(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 - v2}})}(m2)
  override def *(m2: Matrix): Matrix =  dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 * v2}})}(m2)
  override def transpose: Matrix = RealMatrix(m.transpose())
  override def determinant: Double = new LUDecomposition(m).getDeterminant

  override def toString: String = s"Dense real matrix of size ${nrows}x${ncols} - internal: ${m.getRowDimension}x${m.getColumnDimension}"

}

object RealMatrix {

  def apply(a: Array[Array[Double]]): RealMatrix = RealMatrix(linear.MatrixUtils.createRealMatrix(a))

}

/**
  * breeze DenseMatrix
  *  TODO test spark ?
  */
case class BreezeDenseMatrix(m: linalg.DenseMatrix[Double]) extends DenseMatrix {
  override def clone: BreezeDenseMatrix = this.copy(m = linalg.DenseMatrix[Array[Double],Double](values.clone.flatten))//FIXME constructor not correct?

  override def nrows: Int = m.rows
  override def ncols: Int = m.cols

  override def values: Array[Array[Double]] = m.data.grouped(m.cols).toArray
  override def get(i: Int, j: Int): Double = m.valueAt(i,j)
  override def set(i: Int, j: Int, v: Double): Matrix = {
    val d = clone.m
    d.data(d.linearIndex(i,j))=v
    BreezeDenseMatrix(d)
  }

  override def map(f: Double => Double): Matrix = BreezeDenseMatrix(m.map(f))

  def dispatchOp(op: BreezeDenseMatrix => BreezeDenseMatrix): Matrix => Matrix = {
    m2: Matrix => m2 match {
      case m2: BreezeDenseMatrix => op(m2)
      case _ => throw new UnsupportedOperationException("Matrix implementations combination not supported")
    }
  }

  //override def %+%(m2: Matrix): Matrix = dispatchOp{m2=>BreezeDenseMatrix(m+m2.m)}(m2)
  override def %*%(m2: Matrix): Matrix = dispatchOp{m2=>BreezeDenseMatrix(m*m2.m)}(m2)
  override def +(m2: Matrix): Matrix = dispatchOp{m2=>BreezeDenseMatrix(m+m2.m)}(m2)
  override def -(m2: Matrix): Matrix = dispatchOp{m2=>BreezeDenseMatrix(m-m2.m)}(m2)
  override def *(m2: Matrix): Matrix = dispatchOp{m2=>BreezeDenseMatrix(m*:*m2.m)}(m2)
  override def transpose: Matrix = BreezeDenseMatrix(m.t)
  override def determinant: Double = 0.0 // FIXME find LU dec impl in breeze

}

object BreezeDenseMatrix {

  def apply(a: Array[Array[Double]]): DenseMatrix = BreezeDenseMatrix(linalg.DenseMatrix.create(a.length,a(0).length,a.flatten))

}



sealed trait SparseMatrix extends Matrix {
  def nentries: Int
  //def flatValues: Array[Double]
}


object SparseMatrix{

  sealed trait SparseMatrixImplementation
  case class SparseCommons() extends SparseMatrixImplementation
  case class SparseBreeze() extends SparseMatrixImplementation
  private var DEFAULT_SPARSE_MATRIX_IMPLEMENTATION: SparseMatrixImplementation = SparseCommons()
  object SparseMatrixImplementation {
    def setImplSparseCommons: Unit = DEFAULT_SPARSE_MATRIX_IMPLEMENTATION = SparseCommons()
    def setImplSparseBreeze: Unit = DEFAULT_SPARSE_MATRIX_IMPLEMENTATION = SparseBreeze()
  }

  def apply(a: Array[Array[Double]]): SparseMatrix = DEFAULT_SPARSE_MATRIX_IMPLEMENTATION match {
    case _: SparseCommons => SparseMatrixImpl(a)
    case _: SparseBreeze => BreezeSparseMatrix(a)
    case _ => SparseMatrixImpl(a)
  }

  def apply(entries: Array[(Int,Int,Double)],n: Int, p: Int): SparseMatrix = DEFAULT_SPARSE_MATRIX_IMPLEMENTATION match {
    case _: SparseCommons => SparseMatrixImpl(entries,n,p)
    case _: SparseBreeze => BreezeSparseMatrix(entries,n,p)
    case _ => SparseMatrixImpl(entries,n,p)
  }

  /**
    * Row / column matrix
    * @param a
    * @param row
    * @return
    */
  def apply(a: Array[Double], row: Boolean=true): SparseMatrix = if(row) apply(Array(a)) else apply(a.map(Array(_)))

  /**
    * convert from dense - should better have a toSparse function? rq. breeze has built-in toDense
    * @param m
    */
  def apply(m: DenseMatrix): SparseMatrix =  DEFAULT_SPARSE_MATRIX_IMPLEMENTATION match {
    case _: SparseCommons => SparseMatrixImpl(m)
    case _: SparseBreeze => BreezeSparseMatrix(m)
    case _ => SparseMatrixImpl(m)
  }


  /**
    * Random sparse matrix
    * @param n
    * @param p
    * @param density
    * @param rng
    * @return
    */
  def randomSparseMatrix(n: Int, p: Int, density: Double)(implicit rng: Random): SparseMatrix = {
    // this is highly inefficient for high dimensions - sample and reject strategy better
    //val inds: Seq[(Int,Int)] = Stochastic.sampleWithoutReplacement[(Int,Int)](for {i <- 0 until n;j <- 0 until p} yield (i,j), (n*p*density).toInt)
    //val inds = new mutable.HashSet[(Int,Int)]
    val inds = new ArrayBuffer[(Int,Int)]
    def drawInds(numInds: Int): Int = {inds.addOne((rng.nextInt(n),rng.nextInt(p)));inds.size}
    val nentries = (n.toDouble*p.toDouble*density).toInt
    //println(nentries)
    Iterator.iterate(0)(drawInds).takeWhile(_<nentries).toSeq
    val entries = inds.map{case (i,j) => (i,j,rng.nextDouble())}.toArray
    //println(entries.toSeq)
    SparseMatrix(entries,n, p)
  }

}


/**
  * Note: the apache class is kind of crappy as one can not access underlying map and some operations are not implemented
  *  thus should add an additional map here: ~ as memory + perf overhead
  *  BUT terrible to use: reimplement operations, etc
  * @param m
  * @param entries
  */
case class SparseMatrixImpl(m: linear.OpenMapRealMatrix//,
                            //entries: mutable.HashMap[(Int,Int),Double]
                           ) extends SparseMatrix {


  // FIXME this is highly inefficient: for setting one element, the immutable constraint leads to cloning everything!
  // TODO alternative impl and some tests? or switch to mutable :/
  override def clone: SparseMatrixImpl = SparseMatrixImpl(new linear.OpenMapRealMatrix(m))

  override def nrows: Int = m.getRowDimension
  override def ncols: Int = m.getColumnDimension

  override def set(i: Int,j: Int, v: Double): Matrix = {
    val d = this.clone.m
    d.setEntry(i,j,v)
    SparseMatrixImpl(d)
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
    * FIXME does not exploit the sparse structure - should keep the index of non zero elements to iterate only on these?
    *  -> maybe reimplement with proper map would be more straightforward
    * @param f
    * @return
    */
  override def map(f: Double => Double): Matrix = {
    val d = m.getData.clone.map(_.clone)
    SparseMatrixImpl(d.map(_.map(f)))
  }

  /**
    * highly inefficient as getData is in O(n*p) as getData is not reimplemented in OpenMapRealMatrix
    * @return
    */
  override def values: Array[Array[Double]] = m.getData


  def dispatchOp(op: SparseMatrixImpl => SparseMatrixImpl): Matrix => Matrix = {
    m2: Matrix => m2 match {
      case m2: SparseMatrixImpl => op(m2)
      case _ => throw new UnsupportedOperationException("Matrix implementations combination not supported")
    }
  }

  //override def %+%(m2: Matrix): Matrix = dispatchOp {m2 => SparseMatrixImpl(m.add(m2.m))}(m2)
  override def %*%(m2: Matrix): Matrix = dispatchOp {m2 => SparseMatrixImpl(m.multiply(m2.m))}(m2)
  // FIXME check and reimplement, inefficient for now
  override def +(m2: Matrix): Matrix = dispatchOp{m2=>SparseMatrixImpl(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 + v2}})}(m2)
  override def -(m2: Matrix): Matrix = dispatchOp{m2=>SparseMatrixImpl(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 - v2}})}(m2)
  override def *(m2: Matrix): Matrix = dispatchOp{m2=>SparseMatrixImpl(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 * v2}})}(m2)
  // ! transpose not implemented -> transforms into a real matrix
  // cannot go through sparse mat entries: shitty implementation
  override def transpose: Matrix = RealMatrix(m.transpose())
  override def determinant: Double = new linear.LUDecomposition(m).getDeterminant

  override def sum: Double = flatValues.sum
  override def min: Double = flatValues.min(Ordering.Double.TotalOrdering)
  override def max: Double = flatValues.max(Ordering.Double.TotalOrdering)

  //FIXME not optimal?
  override def rowSum: Array[Double] = m.getData.map{_.sum}
  override def colSum: Array[Double] = m.getData.transpose.map{_.sum}

  override def nentries: Int = m.getData.flatten.count(_>0.0)

  /**
    * ! not efficient
    * @return
    */
  override def flatValues: Array[Double] = values.flatten

  override def toString: String = s"Sparse Matrix Apache Impl ${nrows}x${ncols}"

}

object SparseMatrixImpl {

  def apply(entries: Array[(Int,Int,Double)], n: Int, p: Int): SparseMatrixImpl = {
    val m:linear.OpenMapRealMatrix = new linear.OpenMapRealMatrix(n,p)
    entries.foreach{case (i,j,v) => m.setEntry(i,j,v)}
    SparseMatrixImpl(m)
  }

  /**
    * ! this constructor does not make sense - the sparse matrix is full
    * @param a
    * @return
    */
  def apply(a: Array[Array[Double]]): SparseMatrixImpl = {
    val (n,p) = (a.length,a(0).length) //assume the array is actually a matrix (same row size)
    val m:linear.OpenMapRealMatrix = new linear.OpenMapRealMatrix(n,p) // mutable !
    a.zipWithIndex.foreach{case (row,i) => row.zipWithIndex.foreach{case (v,j) => m.setEntry(i,j,v)}}
    SparseMatrixImpl(m)
  }

  /**
    * from a dense matrix - all elements are filled also
    * @param m
    * @return
    */
  def apply(m: DenseMatrix): SparseMatrixImpl = SparseMatrixImpl(m.values)

}


case class BreezeSparseMatrix(m: linalg.CSCMatrix[Double]) extends SparseMatrix {

  // should be copy - immutable clone
  override def clone(): BreezeSparseMatrix = BreezeSparseMatrix(m.copy)

  override def nrows: Int = m.rows
  override def ncols: Int = m.cols

  /**
    *
    * @return
    */
  override def values: Array[Array[Double]] = {
    //not optimal to go through a dense to get values? ~ the way it is implemented
    //BreezeDenseMatrix(m.toDense).values
    val res = Array.fill(nrows,ncols)(0.0)
    m.colPtrs.indices.dropRight(1).foreach{j =>
      val start = m.colPtrs(j)
      val end = m.colPtrs(j+1)
      (start until end).foreach(i => res(m.rowIndices(i))(j) = m.data(i))
    }
    res
  }

  /**
    * specific to the implementation - not efficient as must search in column
    * @param i
    * @param j
    * @return
    */
  override def get(i: Int, j: Int): Double = {
    val start = m.colPtrs(j)
    val end = m.colPtrs(j + 1)
    val ind = util.Arrays.binarySearch(m.rowIndices, start, end, i)
    if (ind < 0 ) 0.0 else m.data(ind)
  }

  override def set(i: Int, j: Int, v: Double): Matrix = {
    val copy = clone()
    copy.m.update(i, j, v) // mutable!
    copy
  }

  override def map(f: Double => Double): Matrix = {
    val d = m.copy
    d.data.map(f) // mutable
    this.copy(m=d)
  }


  def dispatchOp(op: BreezeSparseMatrix => BreezeSparseMatrix): Matrix => Matrix = {
    m2: Matrix => m2 match {
      case m2: BreezeSparseMatrix => op(m2)
      case _ => throw new UnsupportedOperationException("Matrix implementations combination not supported")
    }
  }

  // note: operators / implementations could be passed as implicit context?
  override def %*%(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m*m2.m)}(m2)
  override def *(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m*:*m2.m)}(m2)
  override def +(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m+m2.m)}(m2)
  override def -(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m-m2.m)}(m2)

  override def transpose: Matrix = this.copy(m = m.t)

  // FIXME Breeze LU implementation?
  override def determinant: Double = 0.0

  /**
    * note: same impl as apache sparse mat (flatValues is different)
    * @return
    */
  override def sum: Double = flatValues.sum
  override def min: Double = flatValues.min(Ordering.Double.TotalOrdering)
  override def max: Double = flatValues.max(Ordering.Double.TotalOrdering)

  override def rowSum: Array[Double] = m.data.zip(m.rowIndices).groupBy(_._2).toSeq.map(_._2.map(_._1).sum).toArray
  override def colSum: Array[Double] = Array.tabulate(nrows)(j => util.Arrays.copyOfRange(m.data,m.colPtrs(j), m.colPtrs(j+1)).sum)

  override def nentries: Int = m.data.length

  override def flatValues: Array[Double] = m.data

  override def toString: String = s"Sparse Matrix Breeze Impl ${nrows}x$ncols"

}

object BreezeSparseMatrix {

  /**
    * from list of entries
    * @param entries
    * @param n
    * @param p
    * @return
    */
  def apply(entries: Array[(Int,Int,Double)],n: Int, p: Int): BreezeSparseMatrix = {
    val builder = new CSCMatrix.Builder[Double](rows = n, cols = p)
    entries.foreach {case (i,j,v)=>
      builder.add(i, j, v)
    }
    BreezeSparseMatrix(builder.result())
  }

  /**
    * from values
    * @param a
    * @return
    */
  def apply(a: Array[Array[Double]]): BreezeSparseMatrix = {
    val builder = new CSCMatrix.Builder[Double](rows = a.length, cols = a(0).length)
    a.zipWithIndex.foreach{case (row,i) => row.zipWithIndex.foreach{case (v,j) => builder.add(i,j,v)}}
    BreezeSparseMatrix(builder.result())
  }

  /**
    * from dense matrix
    * @param m
    * @return
    */
  def apply(m: DenseMatrix): BreezeSparseMatrix = apply(m.values)

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



