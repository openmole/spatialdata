package org.openmole.spatialdata.test

import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix, SparseMatrix}

import scala.util.Random
import org.openmole.spatialdata.utils.withTimer

object TestMatrix {


  /**
    *  - sparse mult not performant? recode random generation / count only mult time
    *  - Apache commons impl better than breeze for dense ?
    *  - sparse without gen: 8000 sparse apache: 0.35 ; sparse breeze: 0.25
    *  - sparse with density = 0.8: apache better sparse apache: 0.15 ; sparse breeze: 0.6
    */
  def testImplementations: Unit = {
    implicit val rng = new Random
    implicit val spMatBreeze = SparseMatrix.SparseBreeze()
    implicit val spMatApache = SparseMatrix.SparseCommons()

    val (n,p) = (8000,8000)
    val density = 0.8
    val bootstraps = 20


    val sparseTimes = (0 until bootstraps).map {i =>
      println(i)
      //val (m1,m2) =  (SparseMatrix.randomSparseMatrix(n, p, density),SparseMatrix.randomSparseMatrix(n, 1, density))
      val m1 = SparseMatrix.randomSparseMatrix(n, p, density)(rng, spMatApache)
      val m2 = SparseMatrix(DenseMatrix.ones(n,1))(spMatApache)
      withTimer[Double, Matrix] { _ => m1 %*% m2 }(0.0)._2
    }


    val sparseBreezeTimes = (0 until bootstraps).map {i =>
      println(i)
      //val (m1,m2) = (SparseMatrix.randomSparseMatrix(n, p, density),SparseMatrix.randomSparseMatrix(n, 1, density))
      val m1 = SparseMatrix.randomSparseMatrix(n, p, density)(rng, spMatBreeze)
      val m2 = SparseMatrix(DenseMatrix.ones(n,1))(spMatBreeze)
      withTimer[Double, Matrix] { _ => m1 %*% m2 }(0.0)._2
    }

/*
    val denseTimes = (0 until bootstraps).map {i =>
      println(i)
      //val (m1,m2) = (DenseMatrix.randomDenseMatrix(n, p, density),DenseMatrix.randomDenseMatrix(n, 1, density))
      val m1 = DenseMatrix.randomDenseMatrix(n, p, density)
      val m2 = DenseMatrix.ones(n,1)
      withTimer[Double, Matrix] { _ => m1 %*% m2 }(0.0)._2
    }

    DenseMatrix.DenseMatrixImplementation.setImplDenseBreeze
    val denseBreezeTimes = (0 until bootstraps).map {i =>
      println(i)
      //val (m1,m2) = (DenseMatrix.randomDenseMatrix(n, p, density),DenseMatrix.randomDenseMatrix(n, 1, density))
      val m1 = DenseMatrix.randomDenseMatrix(n, p, density)
      val m2 = DenseMatrix.ones(n,1)
      withTimer[Double, Matrix] { _ => m1 %*% m2 }(0.0)._2
    }
    */


    //println(s"sparse apache: ${sparseTimes.sum/bootstraps} ; sparse breeze: ${sparseBreezeTimes.sum/bootstraps} ; dense apache: ${denseTimes.sum/bootstraps} ; dense breeze: ${denseBreezeTimes.sum/bootstraps}")
    //println(s"dense apache: ${denseTimes.sum/bootstraps} ; dense breeze: ${denseBreezeTimes.sum/bootstraps}")
    println(s"sparse apache: ${sparseTimes.sum/bootstraps} ; sparse breeze: ${sparseBreezeTimes.sum/bootstraps}")


  }


}
