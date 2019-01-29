
package spatialdata.test

import spatialdata.measures.{Morphology, Statistics}
import spatialdata.synthetic.grid.ExpMixtureGenerator

import scala.util.Random

object TestIndicators {

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