package org.openmole.spatialdata.test

import org.openmole.spatialdata.network
import org.openmole.spatialdata.network.{Network, Node, ShortestPaths}
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.network.measures.NetworkMeasures.ShortestPathsNetworkMeasures
import org.openmole.spatialdata.network.synthetic.{GridNetworkGenerator, RandomNetworkGenerator}
import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils.math.Stochastic
import org.openmole.spatialdata.utils._

import scala.util.Random

object TestNetwork {


  /**
    * JGraphT in average around 10 times faster !
    */
  def testShortestPathImplementations(): Unit = {
    implicit val rng = new Random
    val nw = RandomNetworkGenerator(20,200,true,false,false).generateNetwork

    val (deltas,t1,t2) = (0 until 100).map{k =>
      println(k)
      val nodes = Stochastic.sampleWithoutReplacementBy(nw.nodes,{_: Node => 1/nw.nodes.size},10)
      val (sp1,t1) = withTimer[Network,ShortestPaths](nw => GraphAlgorithms.shortestPaths(nw,nodes,{l => l.length}))(nw)
      val (sp2,t2) = withTimer[Network,ShortestPaths](nw => GraphAlgorithms.shortestPathsJGraphT(nw,nodes,{l => l.length}))(nw)
      (sp1 |-| sp2,t1,t2)
    }.unzip3

    println("Total difference = "+deltas.sum)
    println("Time scala graph = "+t1.sum/t1.size)
    println("Time jgraphT = "+t2.sum/t2.size)

  }


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
