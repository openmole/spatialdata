package org.openmole.spatialdata.test

import org.openmole.spatialdata.utils.io.{Binary, CSV}
import org.openmole.spatialdata.utils.math.{Matrix, SparseMatrix, Stochastic}

import scala.util.Random


object TestUtils {


  def testCSVMatrix: Unit = {
    implicit val ord: Ordering[Double] = Ordering.Double.TotalOrdering
    implicit val matImpl: Matrix.MatrixImplementation = Matrix.defaultImplementation
    val path = System.getenv("CS_HOME_EXT2")+"/UrbanDynamics/Data/QUANT/EWS/TObs_1.csv"
    val res = Matrix(CSV.readMat(path))
    println("max="+res.values.map(_.max).max)
    println("min="+res.values.map(_.min).min)
    println("mean="+res.values.flatten.sum / res.values.flatten.length)
  }


  def testBinaryIO: Unit = {
    implicit val ord = Ordering.Double.TotalOrdering
    val path = System.getenv("CS_HOME_EXT2")+"/UrbanDynamics/Data/QUANT/EWS/TObs_1.bin"
    //val res = Binary.readBinaryMatrix(path)
    val res = Binary.readBinary[Matrix](path)
    println("max="+res.values.map(_.max).max)
    println("min="+res.values.map(_.min).min)
    println("mean="+res.values.flatten.sum / res.values.flatten.size)
  }


  def testSampling(): Unit = {

    implicit val rng = new Random

    val samples = 100000

    val sampled = Seq(0.01,0.1,0.09,0.8)
    println(Stochastic.sampleWithReplacementBy[Double](sampled,d => d, samples).groupBy(d=>d).map{g => (g._1,g._2.length.toDouble/samples.toDouble)})

  }


  def testCSV(): Unit = {

    val res = CSV.readCSV("data/test/sample.csv",",")
    println(res)


  }

}
