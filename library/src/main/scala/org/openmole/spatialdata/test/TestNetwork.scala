package org.openmole.spatialdata.test

import org.openmole.spatialdata.network
import org.openmole.spatialdata.network.{Network, Node, ShortestPaths}
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.network.measures.NetworkMeasures.ShortestPathsNetworkMeasures
import org.openmole.spatialdata.network.synthetic.{GridNetworkGenerator, RandomNetworkGenerator, TreeMinDistGenerator}
import org.openmole.spatialdata.utils.graph.GraphAlgorithms.{ScalaGraph, _}
import org.openmole.spatialdata.utils.math.Stochastic
import org.openmole.spatialdata.utils._

import scala.util.Random

object TestNetwork {


  def testTreeMinDist: Unit = {
    implicit val rng = new Random
    val nw = TreeMinDistGenerator(500,connexificationAlgorithm = n => n.projectionConnect).generateNetwork
    //val nw = TreeMinDistGenerator(100).generateNetwork
    visualization.staticVisualization(nw,nodeColorClasses = Some({_ => Seq(1,2,3,4,5,6,7,8).sampleWithReplacement(1).head}))
  }

  /**
    * JGraphT in average around 10 times faster !
    */
  def testShortestPathImplementations(): Unit = {
    implicit val rng = new Random
    val nw = RandomNetworkGenerator(15,50,true,false,false).generateNetwork

    val res = (0 until 100).map{k =>
      if(k%10==0) println(k)
      //val nodes = nw.nodes.sampleWithoutReplacement(10)
      val nodes = nw.nodes.toSeq
      val (sp1,t1) = withTimer[Network,ShortestPaths](nw => shortestPaths(nw,nodes,{l => l.length},ScalaGraph()))(nw)
      val (sp2,t2) = withTimer[Network,ShortestPaths](nw => shortestPaths(nw,nodes,{l => l.length},DijkstraJGraphT()))(nw)
      val (sp3,t3) = withTimer[Network,ShortestPaths](nw => shortestPaths(nw,nodes,{l => l.length},FloydWarshallJGraphT()))(nw)
      (sp1 |-| sp2,t1,t2,t3,nodes.size.toDouble)
    }

    println("Total difference = "+res.map(_._1).sum)
    println("Average nw size = "+res.map(_._5).sum/res.size)
    println("Time scala graph = "+res.map(_._2).sum/res.size)
    println("Time jgraphT = "+res.map(_._3).sum/res.size)
    println("Time jgraphT FloydWarshall = "+res.map(_._4).sum/res.size)
  }


  def testPlanarization(): Unit = {
    implicit val rng = new Random
    val nw = RandomNetworkGenerator(20,200,false,false,false).generateNetwork
    //val nw = GridNetworkGenerator(10).generateNetwork

    //println(nw.links.size)
    //println(network.isPlanar(nw)) // rq: the grid network should already be planar: OK
    // but the random not: WTF?

    val planarized = nw.planarize
    //println(planarized)
    //println(network.isPlanar(planarized))
    //println(planarized.links.map{_.length})

    val measures = ShortestPathsNetworkMeasures(planarized)
    println(measures.toString)


  }


}
