package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.vector.Point
import org.openmole.spatialdata.network._
import org.openmole.spatialdata.vector.synthetic.RandomPointsGenerator
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random

/**
  *
  * @param nnodes number of nodes
  * @param nlinks number of links
  * @param planarize planarize the final network
  * @param directed is the network directed
  * @param withIndex should the nodes be indexed
  * @param points optional nodes
  */
case class RandomNetworkGenerator(
                                   nnodes: Int = 0,
                                   nlinks: Int,
                                   planarize: Boolean = false,
                                   directed: Boolean = false,
                                   withIndex: Boolean = true,
                                   points: Seq[Point] = Seq.empty
                                 ) extends NetworkGenerator {
   override def generateNetwork(implicit rng: Random): Network = RandomNetworkGenerator.randomNetwork(nnodes,nlinks,planarize,directed,withIndex,points)
}


object RandomNetworkGenerator {

  /**
    * planar undirected
    * @param points nodes
    * @param nlinks number of links
    * @return
    */
  def apply(points: Seq[(Double,Double)], nlinks: Int): RandomNetworkGenerator = RandomNetworkGenerator(0,nlinks,planarize = true,directed = false,withIndex = false,points)

  def apply(nnodes: Int, nlinks: Int): RandomNetworkGenerator = RandomNetworkGenerator(nnodes,nlinks,planarize = true,directed = false,withIndex = false, Seq.empty)

  /**
    * basic random euclidian network (no planarisation)
    * @param nnodes number of nodes
    * @param nlinks number of links
    * @return
    */
  def randomNetwork(nnodes: Int,nlinks: Int,planar: Boolean,directed: Boolean,withIndex: Boolean, points: Seq[Point])(implicit rng: Random): Network = {
    if (nnodes==0) Network.empty else {
      val coords = if (points.isEmpty) RandomPointsGenerator(nnodes).generatePoints.asPointSeq else points
      val nodes = if (withIndex) Network(coords.zipWithIndex.map { case ((x, y), id) => Node(id, x, y) }.toSet, Set.empty[Link]) else Network(coords.map { case (x, y) => Node(0, x, y) }.toSet, Set.empty[Link])
      val res = addRandomLinks(nodes, nlinks, directed)
      if (planar) res.planarize else res
    }
  }

  /**
    * Add random links to an existing network
    *
    * @param network network
    * @param nlinks number of links
    * @param rng rng
    * @return
    */
  def addRandomLinks(network: Network,nlinks: Int,directed: Boolean)(implicit rng: Random): Network = {
    if (nlinks==0) network else {
      val (origins, destinations) = (Stochastic.sampleWithReplacement[Node](network.nodes.toSeq, nlinks), Stochastic.sampleWithReplacement[Node](network.nodes.toSeq, nlinks))
      Network(network, origins.zip(destinations).flatMap { case (o, d) => if (o != d) Some(Link(o, d, directed)) else None }.toSet)
    }
  }

}
