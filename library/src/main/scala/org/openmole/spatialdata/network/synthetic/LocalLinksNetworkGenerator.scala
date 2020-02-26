package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.network.{Link, Network, NetworkGenerator}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Stochastic
import org.openmole.spatialdata.vector.Point
import org.openmole.spatialdata.vector.measures.Spatstat

import scala.util.Random


/**
  * Network generator described in
  *
  * Raimbault, J. (2020). Unveiling co-evolutionary patterns in systems of cities: a systematic exploration of the SimpopNet model. In Theories and Models of Urbanization (pp. 261-278). Springer, Cham.
  *
  *  Constructs first a tree network (rq: we use here the TreeMinDist for this, may be different of the original ref),
  *  then adds local links by:
  *   - selecting randomly a node with degree lower than max degree parameter (with no weight here - may be added)
  *   - adding a link to a random neighbor in a given radius
  *   - stop when a certain number of links have been added
  *   - can planarize the final network
  *
  * @param nnodes
  * @param points
  */
case class LocalLinksNetworkGenerator(
                                     nnodes: Int,
                                     points: Seq[Point] = Seq.empty,
                                     addedLinks: Int,
                                     maxDegree: Int,
                                     linkRadius: Double,
                                     planarize: Boolean = true
                                     ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = LocalLinksNetworkGenerator.localLinksNetwork(nnodes,points,addedLinks,maxDegree,linkRadius,planarize)

}


object LocalLinksNetworkGenerator {


  /**
    *
    * @param nnodes
    * @param points
    * @param addedLinks
    * @param maxDegree
    * @param linkRadius
    * @param planarize
    * @param rng
    * @return
    */
  def localLinksNetwork(nnodes: Int, points: Seq[Point], addedLinks: Int, maxDegree: Int, linkRadius: Double, planarize: Boolean)(implicit rng: Random): Network = {
    val basetree = TreeMinDistGenerator(nnodes,points).generateNetwork

    // FIXME maxdegree not used -> sampling not independent anymore, must iterate

    // euclidian dist mat between nodes
    val nodes = basetree.nodes.toSeq
    val nodeIndices = nodes.zipWithIndex.toMap
    val dmat = Spatstat.euclidianDistanceMatrix(nodes.toArray.map(_.position))
    // addedLinks is the max number of links to be added: if max deg gives empty set of nodes, do nothing
    // do not check for duplicated links, very low probability
    val newlinks = Stochastic.sampleWithReplacement(nodes, addedLinks).map{origin =>
      val destination = nodes(Stochastic.sampleWithReplacement(dmat(nodeIndices(origin)).zipWithIndex.filter(_._1 < linkRadius).map(_._2),1).head)
      Link(origin, destination)
    }.toSet
    utils.log(s"added links: ${newlinks}")

    val newnw = basetree.addLinks(newlinks)  // could use LinkAdditionNetworkPerturbator, but shorter directly

    // FIXME planarize is failing
    //if(planarize) newnw.planarize else newnw
    newnw
  }


}
