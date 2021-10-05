package org.openmole.spatialdata.test

import java.io.File

import org.locationtech.jts.io.WKTWriter
import org.openmole.spatialdata.utils.io.{Binary, CSV, GIS, GeoPackage}
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix, Stochastic}

import scala.util.Random


object TestUtils {


  def testRasterInput(): Unit = {
    //val path = System.getenv("CS_HOME")+"/Data/JRC_EC/GHS/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0.tif"
    val path = System.getenv("CS_HOME")+"/Data/JRC_EC/GHS/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0/idf.tif"
    if (new File(path).exists()){
      val raster: Array[Array[Double]] = GIS.readRaster(path)
      println(raster.map(_.toSeq).toSeq)
    }
  }

  def testGeopackageInput(): Unit = {
    val path = System.getenv("CS_HOME")+"/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg"
    if (new File(path).exists()){
      val features = GeoPackage.readGeometry(path,featureName = "GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84")
      val wktwriter: WKTWriter = new WKTWriter()
      features.foreach(f => println(wktwriter.write(f._1)))
      println(s"Features: ${features.size}")
    }
  }


  def testCSVMatrix(): Unit = {
    implicit val ord: Ordering[Double] = Ordering.Double.TotalOrdering
    implicit val matImpl: Matrix.MatrixImplementation = Matrix.defaultImplementation
    val path = System.getenv("CS_HOME_EXT2")+"/UrbanDynamics/Data/QUANT/EWS/TObs_1.csv"
    val res = Matrix(CSV.readMat(path))
    println("max="+res.values.map(_.max).max)
    println("min="+res.values.map(_.min).min)
    println("mean="+res.values.flatten.sum / res.values.flatten.length)
  }


  def testBinaryIO(): Unit = {
    implicit val ord: Ordering[Double] = Ordering.Double.TotalOrdering
    implicit val rng: Random = new Random
    //val path = System.getenv("CS_HOME_EXT2")+"/UrbanDynamics/Data/QUANT/EWS/TObs_1.bin"
    val path = "data/test/testmat.bin"
    Binary.writeBinary(DenseMatrix.randomDenseMatrix(10,10,0.5),path)
    val res = Binary.readBinary[Matrix](path)
    println("max="+res.values.map(_.max).max)
    println("min="+res.values.map(_.min).min)
    println("mean="+res.values.flatten.sum / res.values.flatten.length)
  }


  def testSampling(): Unit = {

    implicit val rng: Random = new Random

    val samples = 100000

    val sampled = Seq(0.01,0.1,0.09,0.8)
    println(Stochastic.sampleWithReplacementBy[Double](sampled,d => d, samples).
      groupBy(d=>d).map{g: (Double,Vector[Double]) => (g._1,g._2.length.toDouble/samples.toDouble)})

  }


  def testCSV(): Unit = {
    val res = CSV.readCSV("data/test/sample1.csv")
    println(res.head)
  }

}
