package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.Point2D
import org.openmole.spatialdata.network.{Network, NetworkGenerator}
import org.openmole.spatialdata.points.synthetic.RandomPointsGenerator

import scala.util.Random

/**
  * A tree network connected iteratively by minimal distance between weak components
  * @param nnodes
  * @param points
  */
case class TreeMinDistGenerator(
                                nnodes: Int,
                                points: Seq[Point2D] = Seq.empty
                               ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = TreeMinDistGenerator.treeMinDistNetwork(nnodes, points)

}



object TreeMinDistGenerator {

  def treeMinDistNetwork(nodes: Int, points: Seq[Point2D])(implicit rng: Random): Network = {
    val coords = if(points.isEmpty) RandomPointsGenerator(nodes).generatePoints else points
    // TODO option to connect each node with closest neighbor before ? (cf NetLogo implementation)
    Network(coords).weakComponentConnect
  }

}
