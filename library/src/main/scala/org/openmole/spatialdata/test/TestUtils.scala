
package org.openmole.spatialdata.test

import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random

object TestUtils {

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