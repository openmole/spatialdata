package org.openmole.spatialdata.test

import org.openmole.spatialdata.utils.math.Convolution
import org.openmole.spatialdata.utils._

import scala.util.Random

object TestMaths {



  def testConvolution(): Unit = {

    val rng = new Random

    // note: in very small array size, direct convol is slightly better
    // we can have |k|>|x|
    val x = Array.fill(100){rng.nextDouble()}
    //val x = Array.fill(10)(1.0)
    val k = Array.fill(121){rng.nextDouble()}
    //val k = Array.fill(3)(1.0)
    val (cx,t1) = withTimer[Array[Double],Array[Double]]{Convolution.convolution(_,k)}(x)
    val (cdx,t2) = withTimer[Array[Double],Array[Double]]{Convolution.directConvol(_,k)}(x)
    //println(cx.toSeq)
    //println(cdx.toSeq)
    println(t1)
    println(t2)

    val convdiff = cx.zip(cdx).map{case (c1,c2) => scala.math.abs(c1-c2)}.sum
    println(convdiff)


  }

}
