package org.openmole.spatialdata.test

import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix, SparseMatrix}

import scala.util.Random
import org.openmole.spatialdata.utils.withTimer

object TestMatrix {

  def testImplementations: Unit = {
    implicit val rng = new Random

    val (n,p) = (1000,1000)
    val density = 0.1
    val bootstraps = 20

    val sparseTimes = (0 until bootstraps).map {i =>
      println(i)
      withTimer[Double, Matrix] { _ => SparseMatrix.randomSparseMatrix(n, p, density) %*% SparseMatrix.randomSparseMatrix(n, 1, density) }(0.0)._2
    }

    val denseTimes = (0 until bootstraps).map {i =>
        println(i)
      withTimer[Double, Matrix] { _ => DenseMatrix.randomDenseMatrix(n, p, density) %*% DenseMatrix.randomDenseMatrix(n, 1, density) }(0.0)._2
    }

    SparseMatrix.SparseMatrixImplementation.setImplSparseBreeze
    val sparseBreezeTimes = (0 until bootstraps).map {i =>
      println(i)
      withTimer[Double, Matrix] { _ => SparseMatrix.randomSparseMatrix(n, p, density) %*% SparseMatrix.randomSparseMatrix(n, 1, density) }(0.0)._2
    }

    DenseMatrix.DenseMatrixImplementation.setImplDenseBreeze
    val denseBreezeTimes = (0 until bootstraps).map {i =>
      println(i)
      withTimer[Double, Matrix] { _ => DenseMatrix.randomDenseMatrix(n, p, density) %*% DenseMatrix.randomDenseMatrix(n, 1, density) }(0.0)._2
    }

    println(s"sparse apache: ${sparseTimes.sum/bootstraps} ; sparse breeze: ${sparseBreezeTimes.sum/bootstraps} ; dense apache: ${denseTimes.sum/bootstraps} ; dense breeze: ${denseBreezeTimes.sum/bootstraps}")

  }


}
