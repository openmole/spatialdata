package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.vector.Point
import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.vector.synthetic.RandomPointsGenerator

import scala.util.Random

/**
  * A tree network connected iteratively by minimal distance between weak components
  * @param nnodes
  * @param points
  */
case class TreeMinDistGenerator(
                                nnodes: Int,
                                points: Seq[Point] = Seq.empty,
                                connexificationAlgorithm: Network => Network = n => n.weakComponentConnect
                               ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = TreeMinDistGenerator.treeMinDistNetwork(nnodes, points, connexificationAlgorithm)

}



object TreeMinDistGenerator {

  /**
    *
    * @param nodes number of random nodes (used if points not empty)
    * @param points coordinates of nodes
    * @param connexificationAlgorithm heuristic to connect
    * @param rng rng
    * @return Tree network
    */
  def treeMinDistNetwork(nodes: Int, points: Seq[Point],connexificationAlgorithm: Network => Network)(implicit rng: Random): Network = {
    val coords = if(points.isEmpty) RandomPointsGenerator(nodes).generatePoints.asPointSeq else points
    connexificationAlgorithm(Network(coords))
  }

}
