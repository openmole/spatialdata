
package spatialdata.test

import spatialdata.synthetic.grid.RandomGridGenerator
import spatialdata.measures.Morphology

import scala.util.Random

object TestSynthetic {


  /**
    * Test : stat distribution of Moran for random grids
    */
  def testRandomGrids(): Unit = {

    implicit val rng = new Random

    val size = 25

    // test empirical distrib of moran for random grids

    val morans = (1 until 10000).map{i =>
      if (i%100==0){println(i)}
      Morphology.moran(RandomGridGenerator(size).generateGrid)
    }.toArray

    import org.dianahep.histogrammar._
    val hist = Bin(50,morans.min,morans.max,{d: Double=>d})
    for (d <- morans) hist.fill(d)

    //issue intelij compil ?
    import org.dianahep.histogrammar.ascii._
    println(hist.ascii)

  }

}


