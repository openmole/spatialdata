package org.openmole.spatialdata.utils.math

import java.util

import org.apache.commons.math3.linear
import org.apache.commons.math3.linear.{LUDecomposition, MatrixUtils}
import breeze.linalg
import breeze.linalg._
import breeze.util.ArrayUtil
import org.openmole.spatialdata.utils

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Generic methods for double matrices
  *   rq: could make generic in Numeric type?
  */
sealed trait Matrix {

  /**
    * rq: in immutable terms, setting is not efficient (should clone the underlying matrix?)
    * @param i row
    * @param j column
    * @param v value
    * @return cloned matrix with value set
    */
  def set(i: Int, j: Int, v: Double): Matrix

  /**
    * Mutable set
    * @param i row
    * @param j column
    * @param v value
    */
  def setM(i: Int, j: Int, v: Double): Unit

  /**
    * get element
    * @param i row
    * @param j column
    * @return value
    */
  def get(i: Int, j: Int): Double

  /**
    * get submatrix
    * @param i starting row
    * @param j starting column
    * @param nrows number of rows
    * @param ncols number of columns
    * @return submatrix
    */
  def getSubmat(i: Int, j: Int, nrows: Int, ncols: Int): Matrix

  /**
    * get non-continuous submatrix
    * @param rowinds row indices
    * @param colinds column indices
    * @return
    */
  def getSubmat(rowinds: Array[Int], colinds: Array[Int]): Matrix

  def getRow(i: Int): Matrix = getSubmat(i,0,1,ncols)
  def getCol(j: Int): Matrix = getSubmat(0,j,nrows,1)

  /**
    * unoptimal mutable submat set
    * @param i starting row
    * @param j starting col
    * @param a values
    */
  def setMSubmat(i: Int, j: Int, a: Array[Array[Double]]): Unit = {
    a.zipWithIndex.foreach{case (row,k) =>
      row.zipWithIndex.foreach{case (v,l) =>
        if(i+k<nrows&&j+l<ncols) setM(i+k,j+l,v)
      }
    }
  }

  /**
    * map element by element
    * @param f function to map
    * @return
    */
  def map(f: Double => Double): Matrix

  // !! mutable
  def values: Array[Array[Double]]

  def flatValues: Array[Double]

  def nrows: Int
  def ncols: Int

  //basic ring operations (R style for the notation)
  def %*%(m: Matrix): Matrix

  //concatenation
  def ::(m: Matrix, byColumns: Boolean = true): Matrix

  // scalar operations
  def +(d: Double): Matrix = map(_+d)
  def -(d: Double): Matrix = map(_-d)
  def *(d: Double): Matrix = map(_*d)

  def +(m: Matrix): Matrix
  def -(m: Matrix): Matrix
  def *(m: Matrix): Matrix
  def ^(m: Matrix): Matrix

  def transpose: Matrix
  def inverse: Matrix // should use pseudo inverse or restrict?
  def determinant: Double

  def sum: Double
  def mean: Double = sum/(nrows*ncols)
  def min: Double
  def max: Double

  def rowSum: Array[Double]
  def colSum: Array[Double]

  override def clone: Matrix = map(v => v)

}

/**
  * matrix utilities (not linear algebra operations which are in [[Linear]]
  *
  *  - test spark matrices https://spark.apache.org/docs/2.1.2/api/java/org/apache/spark/mllib/linalg/Matrix.html?
  *
  * mutable / immutable matrices?
  * https://medium.com/@eob/how-you-might-create-a-scala-matrix-library-in-a-functional-programming-style-760f8bf6ee6
  *
  * Also Jama for matrices Jama.Matrix
  *
  */
object Matrix {

  sealed trait MatrixImplementation
  case class Dense(denseImpl: DenseMatrix.DenseMatrixImplementation) extends MatrixImplementation
  case class Sparse(sparseImpl: SparseMatrix.SparseMatrixImplementation) extends MatrixImplementation
  case class Empty() extends MatrixImplementation

  val defaultImplementation: MatrixImplementation = Dense(DenseMatrix.Real())
  //private implicit val locDefaultImpl: MatrixImplementation = defaultImplementation

  /**
    * could make this implicit also?
    * @param m matrix
    */
  def getImplementation(m: Matrix): MatrixImplementation = m match {
    case _: RealMatrix => Matrix.Dense(DenseMatrix.Real())
    case _: BreezeDenseMatrix => Matrix.Dense(DenseMatrix.DenseBreeze())
    case _: SparseMatrixImpl => Matrix.Sparse(SparseMatrix.SparseCommons())
    case _: BreezeSparseMatrix => Matrix.Sparse(SparseMatrix.SparseBreeze())
    case _: EmptyMatrix => Empty()
  }

  /**
    *
    * Constructs given implicit implementations
    *
    * @param a array of array
    * @return
    */
  def apply(a: Array[Array[Double]])(implicit matrixImpl: MatrixImplementation): Matrix = matrixImpl match {
    case Dense(dmImpl) => DenseMatrix(a)(dmImpl)
    case Sparse(smImpl) => SparseMatrix(a)(smImpl)
    case _: Empty => EmptyMatrix()
  }


  /**
    * column or row constructor
    * @param a array
    * @param row row or column matrix
    * @param matrixImpl implementation
    * @return matrix
    */
  def apply(a: Array[Double], row: Boolean=true)(implicit matrixImpl: MatrixImplementation): Matrix = if(row) apply(Array(a)) else apply(a.map(Array(_)))

  /*
    * Row bind two matrices
    *   !!! rewrite specific for sparse mats
    */
  //def rbind(a1: Matrix, a2: Matrix): Matrix = Matrix(a1.values++a2.values)

  /*
    * Row bind several matrices
    */
  //def rbind(a: Array[Matrix]): Matrix = a.reduce(rbind)

  /*
    * Column bind two matrices
    */
  //def cbind(a1: Matrix, a2: Matrix): Matrix = Matrix((a1.values.transpose++a2.values.transpose).transpose)

  /*
    * Column bind several matrices
    */
  //def cbind(a: Array[Matrix]): Matrix = a.reduce(cbind)

  def msum: (Matrix,Matrix)=>Matrix = {case m: (Matrix,Matrix) => m._1+m._2}


}


case class EmptyMatrix() extends Matrix {
  override def nrows: Int = 0
  override def ncols: Int = 0
  override def values: Array[Array[Double]] = Array.empty[Array[Double]]
  override def flatValues: Array[Double] = Array.empty[Double]
  override def get(i: Int, j: Int): Double = Double.NaN
  override def getSubmat(i: Int, j: Int, nrows: Int, ncols: Int): Matrix = this
  override def getSubmat(rowinds: Array[Int], colinds: Array[Int]): Matrix = this
  override def set(i: Int, j: Int, v: Double): Matrix = this
  override def setM(i: Int, j: Int, v: Double): Unit = {}
  override def %*%(m: Matrix): Matrix = this
  override def ::(m: Matrix, byColummns: Boolean = true): Matrix = m
  override def *(m: Matrix): Matrix = this
  override def +(m: Matrix): Matrix = this
  override def -(m: Matrix): Matrix = this
  override def ^(m: Matrix): Matrix = this
  override def map(f: Double => Double): Matrix = this
  override def determinant: Double = Double.NaN
  override def inverse: Matrix = this
  override def transpose: Matrix = this
  override def sum: Double = Double.NaN
  override def mean: Double = Double.NaN
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

  sealed trait DenseMatrixImplementation //extends MatrixImplementation
  case class Real() extends DenseMatrixImplementation
  case class DenseBreeze() extends DenseMatrixImplementation

  private implicit val defaultDenseMatrixImplementation: DenseMatrixImplementation = Real()


  /**
    * Construct given an implicit implementation
    * @param a values
    * @param dmImpl implicit implementation
    * @return
    */
  def apply(a: Array[Array[Double]])(implicit dmImpl: DenseMatrixImplementation): DenseMatrix = dmImpl match {
    case _: Real => RealMatrix(a)
    case _: DenseBreeze => BreezeDenseMatrix(a)
    case _ => RealMatrix(a)
  }

  /**
    * zeros
    * @param n rows
    * @param p columns
    * @return
    */
  def zeros(n: Int, p: Int): DenseMatrix = constant(n,p,0.0)

  def ones(n: Int, p: Int): DenseMatrix = constant(n,p,1.0)

  def constant(n: Int, p: Int, v: Double): DenseMatrix = DenseMatrix(Array.fill(n)(Array.fill(p)(v)))

  def diagonal(a: Array[Double]): DenseMatrix = DenseMatrix(Array.tabulate(a.length,a.length){case (i,j) => if(i==j)a(i) else 0.0})

  /**
    * random dense matrix
    * @param n rows
    * @param p columns
    * @param density density
    * @param rng implicit random
    * @return
    */
  def randomDenseMatrix(n: Int, p: Int, density: Double)(implicit rng: Random): DenseMatrix = {
    //val m = zeros(n,p)
    //val inds: Seq[(Int,Int)] = Stochastic.sampleWithoutReplacement[(Int,Int)](for {i <- 0 until n;j <- 0 until p} yield (i,j), (n*p*density).toInt)
    //inds.map{case (i,j) => (i,j,rng.nextDouble)}
    // approximate density by drawing at each step
    val values = (for {_ <- 0 until n
         _ <- 0 until p
         r = rng.nextDouble()
    } yield  if (r < density) 0.0 else rng.nextDouble()).toArray.grouped(p).toArray
    DenseMatrix(values)
  }


}

/**
  * Apache commons real matrix as implementation of dense matrix
  * @param m apache RealMatrix
  */
case class RealMatrix(m: linear.RealMatrix) extends DenseMatrix {

  // the object clone does not clone internal references ?
  override def clone: RealMatrix = RealMatrix(m.getData.clone.map(_.clone))

  override def nrows: Int = m.getRowDimension

  override def ncols: Int = m.getColumnDimension

  override def set(i: Int, j: Int, v: Double): Matrix = {
    val d = clone.m
    d.setEntry(i, j, v)
    RealMatrix(d)
  }

  /**
    * Mutable set
    *
    * @param i row
    * @param j column
    * @param v value
    */
  def setM(i: Int, j: Int, v: Double): Unit = m.setEntry(i, j, v)

  def setSubmatM(i: Int, j: Int, d: Array[Array[Double]]): Unit = m.setSubMatrix(d, i, j)

  def setDiagM(d: Double): Unit = (0 until math.min(nrows, ncols)).foreach(i => setM(i, i, d))

  def setDiagM(a: Array[Double]): Unit = a.zipWithIndex.foreach { case (d, i) => setM(i, i, d) }


  /**
    * !!! using getSubmatrix should give issues with mutable -> necessary to copy (~ unoptimal)
    * @param i starting row
    * @param j starting column
    * @param nrow number of rows
    * @param ncol number of columns
    * @return submatrix
    */
  override def getSubmat(i: Int, j: Int, nrow: Int, ncol: Int): Matrix = {
    val dest: Array[Array[Double]] = Array.fill(nrow,ncol)(0.0)
    m.copySubMatrix((i until i + nrow).toArray, (j until j + ncol).toArray,dest)
    RealMatrix(dest)
  }

  override def getSubmat(rowinds: Array[Int], colinds: Array[Int]): Matrix = {
    val dest: Array[Array[Double]] = Array.fill(rowinds.length,colinds.length)(0.0)
    m.copySubMatrix(rowinds, colinds,dest)
    RealMatrix(dest)
  }

  override def get(i: Int, j: Int): Double = m.getEntry(i,j)

  /**
    * for map, go through each element anyway, cloning is less an issue
    * @param f function
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

  override def %*%(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.multiply(m2.m))}(m2)
  override def +(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 + v2}})}(m2)
  override def -(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 - v2}})}(m2)
  override def *(m2: Matrix): Matrix =  dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 * v2}})}(m2)
  override def ^(m2: Matrix): Matrix =  dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => math.pow(v1,v2)}})}(m2)

  /**
    * Concatenate called by RealMMatrix yields the same
    * @param m2 other Matrix
    * @param byColumns by columns
    * @return
    */
  override def ::(m2: Matrix, byColumns: Boolean = true): Matrix = if (byColumns) {
    if (m.getRowDimension != m2.nrows) throw new RuntimeException(s"Incompatible row dimensions: ${m.getRowDimension} != ${m2.nrows}")
    val res = linear.MatrixUtils.createRealMatrix(m2.nrows, this.ncols+m2.ncols)
    res.setSubMatrix(this.values,0, 0)
    res.setSubMatrix(m2.values, 0, this.ncols + 1)
    res.asInstanceOf[Matrix]
  } else {
    if (m.getColumnDimension != m2.ncols) throw new RuntimeException(s"Incompatible columns dimensions: ${m.getColumnDimension} != ${m2.ncols}")
    val res = linear.MatrixUtils.createRealMatrix(this.nrows + m2.nrows, this.ncols)
    res.setSubMatrix(this.values,0, 0)
    res.setSubMatrix(m2.values, this.nrows + 1, 0)
    res.asInstanceOf[Matrix]
  }

  override def transpose: Matrix = RealMatrix(m.transpose())
  override def determinant: Double = new LUDecomposition(m).getDeterminant
  override def inverse: Matrix = RealMatrix(MatrixUtils.inverse(m))

  override def toString: String = s"Dense real matrix of size ${nrows}x$ncols - internal: ${m.getRowDimension}x${m.getColumnDimension}"

}

object RealMatrix {

  def apply(a: Array[Array[Double]]): RealMatrix = RealMatrix(linear.MatrixUtils.createRealMatrix(a))
  def apply(a: Array[Double],row: Boolean=true): RealMatrix = if(row) apply(Array(a)) else apply(a.map(Array(_)))

  def zeros(n: Int, p: Int): RealMatrix = constant(n,p,0.0)
  def ones(n: Int, p: Int): RealMatrix = constant(n,p,1.0)
  def constant(n: Int, p: Int, v: Double): RealMatrix = RealMatrix(Array.fill(n)(Array.fill(p)(v)))


}

/**
  * breeze DenseMatrix
  */
case class BreezeDenseMatrix(m: linalg.DenseMatrix[Double]) extends DenseMatrix {
  override def clone: BreezeDenseMatrix = this.copy(
    m = linalg.DenseMatrix[Array[Double],Double](values.map(_.clone).clone.flatten)
  )

  override def nrows: Int = m.rows
  override def ncols: Int = m.cols

  override def values: Array[Array[Double]] = m.data.grouped(m.cols).toArray
  override def get(i: Int, j: Int): Double = m.valueAt(i,j)
  override def getSubmat(i: Int, j: Int, nrow: Int, ncol: Int): Matrix =
    BreezeDenseMatrix(m(i until i + nrow,j until j + ncol))

  override def getSubmat(rowinds: Array[Int], colinds: Array[Int]): Matrix = {
    val values: Array[Array[Double]] = Array.fill(rowinds.length, colinds.length)(0.0)
    for {i <- rowinds; j <- colinds}{values(i)(j) = m(i,j)}
    BreezeDenseMatrix(values)
  }

  override def set(i: Int, j: Int, v: Double): Matrix = {
    val d = clone.m
    d.data(d.linearIndex(i,j))=v
    BreezeDenseMatrix(d)
  }
  override def setM(i: Int, j: Int, v: Double): Unit = m.data(m.linearIndex(i,j))=v

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
  override def ^(m2: Matrix): Matrix = dispatchOp{m2=>BreezeDenseMatrix(m^:^m2.m)}(m2)
  override def transpose: Matrix = BreezeDenseMatrix(m.t)

  /**
    *  !! not implemented
    * @param m2 m2
    * @return
    */
  override def ::(m2: Matrix, byColumns: Boolean = true): Matrix = EmptyMatrix()

  /**
    * https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet
    * @return
    */
  override def determinant: Double = det(m)
  override def inverse: Matrix = BreezeDenseMatrix(inv(m))

}

object BreezeDenseMatrix {

  def apply(a: Array[Array[Double]]): DenseMatrix = BreezeDenseMatrix(linalg.DenseMatrix.create(a.length,a(0).length,a.flatten))

}



sealed trait SparseMatrix extends Matrix {
  def nentries: Int
  //def flatValues: Array[Double]
}


object SparseMatrix{

  sealed trait SparseMatrixImplementation //extends MatrixImplementation // better not to extend actually, for exhaustive matchs
  case class SparseCommons() extends SparseMatrixImplementation
  case class SparseBreeze() extends SparseMatrixImplementation

  private implicit val defaultSparseImplementation: SparseMatrixImplementation = SparseBreeze()

  def apply(a: Array[Array[Double]])(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix = spMatImpl match {
    case _: SparseCommons => SparseMatrixImpl(a)
    case _: SparseBreeze => BreezeSparseMatrix(a)
    case _ => SparseMatrixImpl(a)
  }

  def apply(entries: Array[(Int,Int,Double)],
            n: Int,
            p: Int
           )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix = spMatImpl match {
    case _: SparseCommons => SparseMatrixImpl(entries,n,p)
    case _: SparseBreeze => BreezeSparseMatrix(entries,n,p)
    case _ => SparseMatrixImpl(entries,n,p)
  }

  /**
    * Row / column matrix
    * @param a data
    * @param row row matrix?
    * @return
    */
  def apply(a: Array[Double], row: Boolean=true)(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix =
    if(row) apply(Array(a))(spMatImpl) else apply(a.map(Array(_)))(spMatImpl)

  /**
    * convert from dense - should better have a toSparse function? rq. breeze has built-in toDense
    * @param m DenseMatrix
    */
  def apply(m: DenseMatrix)(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix =  spMatImpl match {
    case _: SparseCommons => SparseMatrixImpl(m)
    case _: SparseBreeze => BreezeSparseMatrix(m)
    case _ => SparseMatrixImpl(m)
  }


  /**
    * Random sparse matrix
    * @param n rows
    * @param p columns
    * @param density density
    * @param rng implicit random
    * @return
    */
  def randomSparseMatrix(n: Int, p: Int, density: Double)(implicit rng: Random, spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix = {
    // this is highly inefficient for high dimensions - sample and reject strategy better
    //val inds: Seq[(Int,Int)] = Stochastic.sampleWithoutReplacement[(Int,Int)](for {i <- 0 until n;j <- 0 until p} yield (i,j), (n*p*density).toInt)
    //val inds = new mutable.HashSet[(Int,Int)]
    val inds = new ArrayBuffer[(Int,Int)]
    def drawInds: Int => Int = _ => {inds.addOne((rng.nextInt(n),rng.nextInt(p)));inds.size}
    val nentries = (n.toDouble*p.toDouble*density).toInt
    //println(nentries)
    Iterator.iterate(0)(drawInds).takeWhile(_<nentries).toSeq
    val entries = inds.map{case (i,j) => (i,j,rng.nextDouble())}.toArray
    //println(entries.toSeq)
    SparseMatrix(entries,n, p)(spMatImpl)
  }

  def diagonal(a: Array[Double])(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix =
    apply(a.zipWithIndex.map{case (v,i) => (i,i,v)},a.length,a.length)(spMatImpl)


}


/**
  * Note: the apache class is kind of crappy as one can not access underlying map and some operations are not implemented
  *  thus should add an additional map here: ~ as memory + perf overhead
  *  BUT terrible to use: reimplement operations, etc
  * @param m apache sparse mat
  */
case class SparseMatrixImpl(m: linear.OpenMapRealMatrix//,
                            //entries: mutable.HashMap[(Int,Int),Double]
                           ) extends SparseMatrix {


  /**
    * this is highly inefficient: for setting one element, the immutable constraint leads to cloning everything!
    * alternative impl and some tests? or use mutable when necessary
    * @return
    */
  override def clone: SparseMatrixImpl = SparseMatrixImpl(new linear.OpenMapRealMatrix(m))

  override def nrows: Int = m.getRowDimension
  override def ncols: Int = m.getColumnDimension

  override def set(i: Int,j: Int, v: Double): Matrix = {
    val d = this.clone.m
    d.setEntry(i,j,v)
    SparseMatrixImpl(d)
  }

  override def setM(i: Int, j: Int, v: Double): Unit = m.setEntry(i,j,v)

  override def get(i: Int, j: Int): Double = m.getEntry(i,j)

  /**
    * ! not efficient, using getData
    * @param i starting row
    * @param j starting column
    * @param nrows number of rows
    * @param ncols number of columns
    *  @return submatrix
    */
  override def getSubmat(i: Int, j: Int, nrows: Int, ncols: Int): Matrix =
     SparseMatrixImpl(m.getSubMatrix((i until i + nrows).toArray,(j until j + ncols).toArray).getData)

  /**
    * ! not efficient
    * @param rowinds row indices
    * @param colinds column indices
    *  @return
    */
  override def getSubmat(rowinds: Array[Int], colinds: Array[Int]): Matrix =
    SparseMatrixImpl(m.getSubMatrix(rowinds,colinds).getData)

  /**
    * !!! does not exploit the sparse structure - should keep the index of non zero elements to iterate only on these?
    *  -> maybe reimplement with proper map would be more straightforward
    * @param f function
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

  /**
    * !!! check and reimplement, inefficient for now
    * @param m2 other matrix
    * @return
    */
  override def +(m2: Matrix): Matrix = dispatchOp{m2=>SparseMatrixImpl(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 + v2}})}(m2)
  override def -(m2: Matrix): Matrix = dispatchOp{m2=>SparseMatrixImpl(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 - v2}})}(m2)
  override def *(m2: Matrix): Matrix = dispatchOp{m2=>SparseMatrixImpl(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 * v2}})}(m2)
  override def ^(m2: Matrix): Matrix = dispatchOp{m2=>SparseMatrixImpl(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => math.pow(v1, v2)}})}(m2)

  /**
    *  !! not implemented
    * @param m2 m2
    * @return
    */
  override def ::(m2: Matrix, byColumns: Boolean = true): Matrix = EmptyMatrix()


  // ! transpose not implemented -> transforms into a real matrix
  // cannot go through sparse mat entries: shitty implementation
  override def transpose: Matrix = RealMatrix(m.transpose())
  override def determinant: Double = new linear.LUDecomposition(m).getDeterminant
  override def inverse: Matrix = RealMatrix(MatrixUtils.inverse(m))

  override def sum: Double = flatValues.sum
  override def min: Double = flatValues.min(Ordering.Double.TotalOrdering)
  override def max: Double = flatValues.max(Ordering.Double.TotalOrdering)

  /**
    *   not optimal
    */
  override def rowSum: Array[Double] = m.getData.map{_.sum}
  override def colSum: Array[Double] = m.getData.transpose.map{_.sum}

  override def nentries: Int = m.getData.flatten.count(_>0.0)

  /**
    * ! not efficient
    * @return
    */
  override def flatValues: Array[Double] = values.flatten

  override def toString: String = s"Sparse Matrix Apache Impl ${nrows}x$ncols"

}

object SparseMatrixImpl {

  def apply(entries: Array[(Int,Int,Double)], n: Int, p: Int): SparseMatrixImpl = {
    val m:linear.OpenMapRealMatrix = new linear.OpenMapRealMatrix(n,p)
    entries.foreach{case (i,j,v) => m.setEntry(i,j,v)}
    SparseMatrixImpl(m)
  }

  /**
    * ! this constructor does not make sense - the sparse matrix is full
    * @param a data
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
    * @param m dense matrix
    * @return
    */
  def apply(m: DenseMatrix): SparseMatrixImpl = SparseMatrixImpl(m.values)

}


case class BreezeSparseMatrix(m: linalg.CSCMatrix[Double]) extends SparseMatrix {

  // should be copy - immutable clone
  override def clone: BreezeSparseMatrix = BreezeSparseMatrix(m.copy)
  def dense: linalg.DenseMatrix[Double] = m.toDense

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
    * @param i row
    * @param j column
    * @return value
    */
  override def get(i: Int, j: Int): Double = {
    val start = m.colPtrs(j)
    val end = m.colPtrs(j + 1)
    val ind = util.Arrays.binarySearch(m.rowIndices, start, end, i)
    if (ind < 0 ) 0.0 else m.data(ind)
  }

  /**
    * get submatrix
    *  Rq: going through a dense is totally inefficient
    *
    * @param i starting row
    * @param j starting column
    * @param nrows number of rows
    * @param ncols number of columns
    *  @return submatrix
    */
  override def getSubmat(i: Int, j: Int, nrows: Int, ncols: Int): Matrix = getSubmat((i until (i + nrows)).toArray, (j until (j + ncols)).toArray)

  /**
    * rebuild a sparse submat in O(nelements)
    *
    * @param rowinds row indices
    * @param colinds column indices
    *  @return
    */
  override def getSubmat(rowinds: Array[Int], colinds: Array[Int]): Matrix = {
    //utils.log(s"Extracting submatrix of size ${rowinds.length}x${colinds.length} from BreezeSparseMatrix of size ${m.rows}x${m.cols}")
    val builder = new CSCMatrix.Builder[Double](rows = rowinds.length, cols = colinds.length)
    val (rowMap, colMap) = (rowinds.zipWithIndex.toMap, colinds.zipWithIndex.toMap)
    m.colPtrs.indices.dropRight(1).foreach{j =>
      if (colinds.contains(j)) {
        val start = m.colPtrs(j)
        val end = m.colPtrs(j + 1)
        (start until end).foreach{k =>
          val i = m.rowIndices(k)
          if (rowinds.contains(i)) builder.add(rowMap(i), colMap(j), m.data(k))
        }
      }
    }
    BreezeSparseMatrix(builder.result())
  }

  override def set(i: Int, j: Int, v: Double): Matrix = {
    val copy = clone()
    copy.m.update(i, j, v) // mutable!
    copy
  }
  override def setM(i: Int, j: Int, v: Double): Unit = m.update(i, j, v)

  /**
    * the map implementation does on all dense values?
    * @param f function
    * @return
    */
  override def map(f: Double => Double): Matrix = {
    val mapped = new CSCMatrix[Double](ArrayUtil.copyOf(m.data, m.activeSize).map(f), m.rows, m.cols, m.colPtrs.clone, m.activeSize, m.rowIndices.clone)
    BreezeSparseMatrix(mapped)
  }


  def dispatchOp(op: BreezeSparseMatrix => BreezeSparseMatrix): Matrix => Matrix = {
    m2: Matrix => m2 match {
      case m2: BreezeSparseMatrix => op(m2)
      case _ => throw new UnsupportedOperationException("Matrix implementations combination not supported")
    }
  }

  /**
    * try specific implementation for ebe multiply
    *  the new sparse mat will at least have less elements than both
    *
    *  !!! HUGE overhead, much less good than the native impl which actually uses sparse
    *
    * @param bm2 other matrix
    * @return
    */
  def eBeMultiply(bm2: BreezeSparseMatrix): BreezeSparseMatrix = {
    val m2 = bm2.m
    val data = new ArrayBuffer[Double]
    val rowInds = new ArrayBuffer[Int]
    val colPtrs = new ArrayBuffer[Int]
    colPtrs.addOne(0)
    m.colPtrs.indices.dropRight(1).foreach{ j =>
      val (start1,end1,start2,end2) = (m.colPtrs(j),m.colPtrs(j+1),m2.colPtrs(j),m2.colPtrs(j+1))
      val inds1 = util.Arrays.copyOfRange(m.rowIndices,start1,end1).toSet //  test if not too much perf loss with toSet ?
      val inds2 = util.Arrays.copyOfRange(m2.rowIndices,start2,end2).toSet
      val inds1Map = inds1.zipWithIndex.toMap
      val inds2Map = inds2.zipWithIndex.toMap
      val support = inds1.intersect(inds2)
      rowInds.appendAll(support) // update rowinds
      support.foreach(row => data.addOne(m.data(inds1Map(row))*m2.data(inds2Map(row)))) // update data
      colPtrs.addOne(data.size)
    }
    BreezeSparseMatrix(new CSCMatrix[Double](data.toArray, m.rows, m.cols, colPtrs.toArray, data.size, rowInds.toArray))
  }

  // note: operators / implementations could be passed as implicit context?
  override def %*%(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m*m2.m)}(m2)
  override def *(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m*:*m2.m)}(m2)
  override def +(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m+m2.m)}(m2)
  override def -(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m-m2.m)}(m2)
  override def ^(m2: Matrix): Matrix = dispatchOp {m2 => BreezeSparseMatrix(m^:^m2.m)}(m2)

  /**
    *  !! not implemented
    * @param m2 m2
    * @return
    */
  override def ::(m2: Matrix, byColumns: Boolean = true): Matrix = EmptyMatrix()


  override def transpose: Matrix = this.copy(m = m.t)

  /**
    * Converts to dense to compute det
    * @return
    */
  override def determinant: Double = det(dense)

  /**
    * Converts to dense to comput inverse
    * @return
    */
  override def inverse: Matrix = BreezeDenseMatrix(inv(dense))

  /**
    * note: same impl as apache sparse mat (flatValues is different)
    * @return
    */
  override def sum: Double = flatValues.sum
  override def min: Double = flatValues.min(Ordering.Double.TotalOrdering)
  override def max: Double = flatValues.max(Ordering.Double.TotalOrdering)

  /**
    * rq: empty rows must be taken into account
    * @return
    */
  override def rowSum: Array[Double] = {
    val nonemptyrows: Map[Int,Double] = m.data.zip(m.rowIndices).groupBy(_._2).toSeq.map(r => (r._1,r._2.map(_._1).sum)).toMap
    Array.tabulate(nrows){i => nonemptyrows.getOrElse(i,0.0)}
  }

  /**
    * all cols are included in colPtrs (empty subset if column is empty)
    * @return
    */
  override def colSum: Array[Double] = Array.tabulate(nrows)(j => util.Arrays.copyOfRange(m.data,m.colPtrs(j), m.colPtrs(j+1)).sum)

  override def nentries: Int = m.data.length

  /**
    * Note for a row / column matrix, equal to number of elements only if no zeros - otherwise use values.flatten
    * @return
    */
  override def flatValues: Array[Double] = m.data

  override def toString: String = s"Sparse Matrix Breeze Impl ${nrows}x$ncols with $nentries entries"

}

object BreezeSparseMatrix {

  /**
    * from list of entries
    * @param entries entries
    * @param n rows
    * @param p columns
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
    * @param a values
    * @return
    */
  def apply(a: Array[Array[Double]]): BreezeSparseMatrix = {
    val builder = new CSCMatrix.Builder[Double](rows = a.length, cols = a(0).length)
    a.zipWithIndex.foreach{case (row,i) => row.zipWithIndex.foreach{case (v,j) => builder.add(i,j,v)}}
    BreezeSparseMatrix(builder.result())
  }

  /**
    * from dense matrix
    * @param m dense matrix
    * @return
    */
  def apply(m: DenseMatrix): BreezeSparseMatrix = apply(m.values)

}






/**
  * although not super efficient before matrix operations, string indices can be used for correspondance between dimensions
  * @param m matrix
  * @param rowNames row names
  * @param colNames column names
  */
case class IndexedMatrix(
                        m: Matrix,
                        rowNames: Seq[String],
                        colNames: Seq[String]
                        )



