
package spatialdata.test

import spatialdata._
import spatialdata.measures.{Morphology, Spatstat, Statistics}
import spatialdata.synthetic.grid.ExpMixtureGenerator
import spatialdata.utils.io.CSV

import scala.util.Random

object TestIndicators {


  def testSpatialIndics(): Unit = {
    val data2 = CSV.readCSV("data/test/sample2.csv",",")
    val data1 = CSV.readCSV("data/test/sample1.csv",",")
    // variable 1 : v ; position : c ; variable 2 : Delta h : h
    //println(data1("v"))
    val r = data2("h").zip(data1("h")).map{case (h2,h1)=> h2.toDouble - h1.toDouble}.zip(data1("v")).map{case (deltah,v)=> deltah/v.toDouble}
    val points: Array[Point2D] = data1("c").map{case s => {val a = s.replace("(","").replace(")","").split(",");(a(0).trim.toDouble,a(1).trim.toDouble)}}.toArray

    //println(r)
    //println(points.toSeq)
    //println("Mean dist = "+Spatstat.euclidianDistanceMatrix(points).flatten.sum/(points.length*points.length))

    println("Moran = "+Spatstat.moran(points,r.toArray,
      weightFunction = {p => Spatstat.spatialWeights(p).map{_.map{_*100000}}},
      filter = !_.isNaN
    )
    )

    println(Statistics.histogram(r.toArray,100,filter = !_.isNaN,
      display=true).toSeq)
    println(Statistics.moment(r.toArray,2))
    println(Spatstat.spatialMoment(points,r.toArray,1,1))

  }


  def testStats(): Unit = {
    val rng = new util.Random
    println(Statistics.histogram(Array.fill(1000000){rng.nextDouble()},20).toSeq)//.map{case c => Array(c._1,c._2)})
  }

  def testMoran(): Unit = {

    implicit val rng = new Random

    val gen = ExpMixtureGenerator(Left(100),10,1.0,10.0)

    val morans = (1 to 1000).map{i =>
      println(i)
      Morphology.moran(gen.generateGrid)
    }.toArray

    import org.dianahep.histogrammar._
    val hist = Bin(50,morans.min,morans.max,{d: Double=>d})
    for (d <- morans) hist.fill(d)
    import org.dianahep.histogrammar.ascii._
    println(hist.ascii)


  }


}