package org.openmole.spatialdata.test

import org.openmole.spatialdata.network
import org.openmole.spatialdata.network.Network
import org.openmole.spatialdata.network.synthetic.{GridNetworkGenerator, RandomNetworkGenerator}

import scala.util.Random

object TestNetwork {


  def testPlanarization(): Unit = {
    implicit val rng = new Random
    val nw = RandomNetworkGenerator(30,200,false,false,false).generateNetwork
    //val nw = GridNetworkGenerator(10).generateNetwork

    //println(nw.links.size)
    println(network.isPlanar(nw)) // rq: the grid network should already be planar: OK
    // but the random not: WTF?

    val planarized = network.planarize(nw)
    println(planarized)
    println(network.isPlanar(planarized))


  }


}
