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
                                   nlinks: Int
                                 ) extends NetworkGenerator {
  override def generateNetwork(implicit rng: Random): Network = RandomNetworkGenerator.randomNetwork(nnodes,nlinks)
}


object RandomNetworkGenerator {


  /**
    * basic random euclidian network (no planarisation)
    * @param nnodes
    * @param nlinks
    * @return
    */
  def randomNetwork(nnodes: Int,nlinks: Int)(implicit rng: Random): Network = {
    val coords = RandomPointsGenerator(nnodes).generatePoints
    val nodes = Network(coords.zipWithIndex.map{case ((x,y),id) => Node(id,x,y)}.toSet,Set.empty[Link])
    addRandomLinks(nodes,nlinks)
  }

  /**
    * add random links to an existing network
    * @param network
    * @param nlinks
    * @param rng
    * @return
    */
  def addRandomLinks(network: Network,nlinks: Int)(implicit rng: Random): Network = {
    val (origins,destinations) = (network.nodes.randomTake(nlinks),network.nodes.randomTake(nlinks))
    Network(network,origins.zip(destinations).map{case (o,d) => Link(o,d)}.toSet)
  }

}
