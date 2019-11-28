package org.openmole.spatialdata.test

import better.files.File

import scala.util.Random
import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.measures.GridMorphology.{AverageDistance, Moran}
import org.openmole.spatialdata.vector.measures.SummarySpatialStatistics
import org.openmole.spatialdata.grid.measures._
import org.openmole.spatialdata.grid.synthetic._
import org.openmole.spatialdata.utils._
import org.openmole.spatialdata.utils.io._
import org.openmole.spatialdata.utils.math.{Convolution, Statistics}


object TestIndicators {


  def testFFTConvolution(): Unit = {
    implicit val rng = new Random


    //(0 to 20).foreach{_ =>
      val grid = ExpMixtureGenerator(50,3,1.0,10.0,true).generateGrid
      val maxval = grid.flatten.max
      val ngrid = grid.map{_.map{case d => if(d / maxval > 0.6) 1.0 else 0.0}}

    PNG.write(ngrid, File("data") / "test/grid.png")
    PNG.write(GridMorphology.erosion(ngrid), File("data") / "test/gridFFT.png")
    PNG.write(GridMorphology.erosion(ngrid,Convolution.convolution2dDirect), File("data") / "test/gridDirect.png")
    // TODO fix the fft convolution
    /*
      time(_=>println("fft erosion steps = "+Morphology.fullErosionSteps(ngrid)))
      time(_=>println("direct erosion steps = "+Morphology.fullErosionSteps(ngrid,Morphology.convolutionDirect)))
      time(_=>println("fft dilation steps = "+Morphology.fullDilationSteps(ngrid)))
      time(_=>println("direct dilation steps = "+Morphology.fullDilationSteps(ngrid,Morphology.convolutionDirect)))
    */
    //}

  }




  def testSpatialIndics(): Unit = {
    val data2 = CSV.readCSV("data/test/sample2.csv",",")
    val data1 = CSV.readCSV("data/test/sample1.csv",",")
    // variable 1 : v ; position : c ; variable 2 : Delta h : h
    //println(data1("v"))
    val m = data2("h").zip(data1("h")).map{case (h2,h1)=> h2.toDouble - h1.toDouble}
    val a = data1("v")
    val atot = a.map{_.toDouble}.sum
    val r = m.zip(a).map{case (mm,aa)=> mm/aa.toDouble}
    val weightedAvg = m.zip(a).map{case (mm,aa)=>mm*aa.toDouble/atot}.sum
    val points = data1("c").map{case s => {val a = s.replace("(","").replace(")","").split(",");(a(0).trim.toDouble,a(1).trim.toDouble)}}.toArray

    //println("counts = "+hist.map{_._2}.sum+" / "+r.filter(!_.isNaN).length+" ("+r.filter(_.isNaN).length+" / "+r.size+" NaNs )")
    //println("fully occupied areas : "+r.filter(_==1.0).size)
    //println("no vacancy areas : "+r.filter(_.isNaN).length)

    println(SummarySpatialStatistics(r.toArray,points,{_==1.0}))
    println("Weighted average = "+weightedAvg)
  }


  def testStats(): Unit = {
    val rng = new util.Random
    println(Statistics.histogram(Array.fill(1000000){rng.nextDouble()},20).toSeq)//.map{case c => Array(c._1,c._2)})
  }

  def testMorphology(): Unit = {

    implicit val rng = new Random

    val gen = ExpMixtureGenerator((20,40),10,1.0,10.0)

    val indics = (1 to 100).map{i =>
      println(i)
      val grid = gen.generateGrid
      val (mo1,t1) = withTimer[GridMorphology](GridMorphology(grid,Seq(Moran(),AverageDistance())))
      println(mo1.avgDistance)
      val (mo2,t2) = withTimer[(Double,Double)]((GridMorphology.moranDirect(grid),GridMorphology.distanceMeanDirect(grid)))
      println(mo2._2)
      (mo1,mo2,t1,t2)
    }.toArray

    println(indics.map{d => scala.math.abs(d._1.moran - d._2._1)+scala.math.abs(d._1.avgDistance - d._2._2)}.sum)
    println(indics.map{_._3}.sum/indics.length.toDouble)
    println(indics.map{_._4}.sum/indics.length.toDouble)

    /*
    import org.dianahep.histogrammar._
    import org.dianahep.histogrammar.ascii._

    val hist = Bin(50,morans.min,morans.max,{d: Double=>d})
    for (d <- morans) hist.fill(d)
    println(hist.ascii)
*/


  }


}
