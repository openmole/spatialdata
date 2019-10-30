package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.network._
import org.openmole.spatialdata.points.synthetic.RandomPointsGenerator
import org.openmole.spatialdata.utils._

import scala.util.Random

case class RandomNetworkGenerator(
                                   /**
                                     * number of nodes
                                     */
                                   nnodes: Int,

                                   /**
                                     * number of links
                                     */
                                   nlinks: Int,

                                   /**
                                     * planarize the final network
                                     */
                                   planarize: Boolean = false,

                                   directed: Boolean = false,

                                   withIndex: Boolean = true
                                 ) extends NetworkGenerator {
   override def generateNetwork(implicit rng: Random): Network = RandomNetworkGenerator.randomNetwork(nnodes,nlinks,planarize,directed,withIndex)
}


object RandomNetworkGenerator {


  /**
    * basic random euclidian network (no planarisation)
    * @param nnodes
    * @param nlinks
    * @return
    */
  def randomNetwork(nnodes: Int,nlinks: Int,planar: Boolean,directed: Boolean,withIndex: Boolean)(implicit rng: Random): Network = {
    val coords = RandomPointsGenerator(nnodes).generatePoints
    val nodes = if(withIndex) Network(coords.zipWithIndex.map{case ((x,y),id) => Node(id,x,y)}.toSet,Set.empty[Link]) else Network(coords.map{case (x,y) => Node(0,x,y)}.toSet,Set.empty[Link])
    val res = addRandomLinks(nodes,nlinks,directed)
    if(planar) planarize(res) else res
  }

  /**
    * add random links to an existing network
    * @param network
    * @param nlinks
    * @param rng
    * @return
    */
  def addRandomLinks(network: Network,nlinks: Int,directed: Boolean)(implicit rng: Random): Network = {
    val (origins,destinations) = (network.nodes.randomTake(nlinks),network.nodes.randomTake(nlinks))
    Network(network,origins.zip(destinations).flatMap{case (o,d) => if(o!=d) Some(Link(o,d,directed)) else None}.toSet)
  }

}
