package org.openmole.spatialdata.test

import org.openmole.spatialdata.network
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.network.measures.NetworkMeasures.ShortestPathsNetworkMeasures
import org.openmole.spatialdata.network.synthetic.{GridNetworkGenerator, RandomNetworkGenerator}

import scala.util.Random

object TestNetwork {


  def testPlanarization(): Unit = {
    implicit val rng = new Random
    val nw = RandomNetworkGenerator(20,200,false,false,false).generateNetwork
    //val nw = GridNetworkGenerator(10).generateNetwork

    //println(nw.links.size)
    //println(network.isPlanar(nw)) // rq: the grid network should already be planar: OK
    // but the random not: WTF?

    val planarized = network.planarize(nw)
    //println(planarized)
    //println(network.isPlanar(planarized))
    //println(planarized.links.map{_.length})

    val measures = ShortestPathsNetworkMeasures(planarized)
    println(measures.toString)


  }


}
