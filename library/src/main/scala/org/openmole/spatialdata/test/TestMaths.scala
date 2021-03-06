package org.openmole.spatialdata.test

import org.openmole.spatialdata.grid.synthetic.CorrelatedPercolationGridGenerator
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{Convolution, Stochastic}
import org.openmole.spatialdata.utils.{visualization, withTimer}

import scala.util.Random

object TestMaths {


  def testCumsum(): Unit = {
    val res = utils.math.cumsum(Seq(1,2,1,2,1))
    println("cumsum = "+res)
    assert(res==Seq(1,3,4,6,7))
  }


  def testFourierCorrelatedField(gui: Boolean = true): Unit = {
    implicit val rng: Random = new Random
    val res = CorrelatedPercolationGridGenerator.correlatedField(100, 0.2)
    if (gui) {
      println(res.toSeq.map(_.toSeq))
      visualization.staticRasterVisualization(res)
    }
  }


  def testStochastic(): Unit = {
    implicit val rng: Random = new Random(42)
    val std = 1.5
    val lndraw = Stochastic.LogNormalDistribution(0.0,math.sqrt(math.log(std)+0.5)).draw
    assert(!lndraw.isNaN,s"NaN drawn from log-normal with mu = 0 and std $std")
  }


  def testConvolution2D(): Unit = {
    val rng = new Random
    val x = Array.fill(10,20){rng.nextDouble()}
    val k = Array.fill(5,5)(1.0)
    val c2d = Convolution.convolution2D(x,k)
    println(s"${c2d.length} ; ${c2d(0).length}")

  }



  def testConvolution(quiet: Boolean = false): Unit = {

    val rng = new Random

    // note: in very small array size, direct convol is slightly better
    // we can have |k|>|x|
    val x = Array.fill(100){rng.nextDouble()}
    //val x = Array.fill(10)(1.0)
    val k = Array.fill(61){rng.nextDouble()}
    //val k = Array.fill(3)(1.0)
    val (cx,t1) = withTimer[Array[Double],Array[Double]]{Convolution.convolution(_,k)}(x)
    val (cdx,t2) = withTimer[Array[Double],Array[Double]]{Convolution.directConvol(_,k)}(x)
    //println(cx.toSeq)
    //println(cdx.toSeq)

    if (!quiet) {
      println(t1)
      println(t2)
    }

    val convdiff = cx.zip(cdx).map{case (c1,c2) => scala.math.abs(c1-c2)}.sum

    if (!quiet) println(convdiff)


  }

}
