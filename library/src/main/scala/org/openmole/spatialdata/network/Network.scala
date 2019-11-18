package org.openmole.spatialdata.network

import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils._
import org.openmole.spatialdata.utils.graph.GraphAlgorithms.{DijkstraJGraphT, ShortestPathMethod}

import scala.util.Random
import scala.math._

/**
  * Network
  *
  * TODO - many functions are a mess, in particular adding nodes and links - this must be fixed and made consistent with adjacency representation
  * The sequence order may be used by functions needing it (adjacency matrix operations etc)
  *
  * TODO add directed and weighted options
  *
  * TODO can we have a composition law for networks to have some sort of structure ?
  *
  * @param nodes
  * @param links
  */
case class Network(
                    nodes: Set[Node],
                    links: Set[Link],
                    directed: Boolean = false,
                    cachedShortestPaths: Option[Map[(Node,Node),(Seq[Node],Seq[Link],Double)]] = None,
                    shortestPathsMethod: ShortestPathMethod = DijkstraJGraphT()
                  ) {

  def hasConsistentIds: Boolean = nodes.toSeq.map(_.id).distinct.length==nodes.size

  /**
    * FIXME recompute if link weight function has changed - requires to cache the weight function
    * @param linkWeight
    * @return
    */
  def computeShortestPaths(linkWeight: Link => Double = _.weight,
                           pathSample: Double = 1.0,
                           recompute: Boolean = false)(implicit rng: Random): Network =
    if(cachedShortestPaths.isEmpty||recompute)
      this.copy(cachedShortestPaths=Some(
        GraphAlgorithms.shortestPaths(
          network = this,
          vertices = nodes.sampleWithoutReplacement(floor(pathSample*nodes.size).toInt),
          linkWeight,
          shortestPathsMethod
         ) // FIXME provide context bound
        )
      )
    else this

  /**
    * update link weights
    *   !!! does not change the cached shortest paths
    * @param newLinkCosts
    * @return
    */
  def updateLinkCosts(newLinkCosts: Seq[Link]): Network = {
    val linkCostMap = newLinkCosts.map{l => (l.id,l.weight)}.toMap
    this.copy(links = links.map{l => l.copy(weight=linkCostMap.getOrElse(l.id,l.weight))})
  }


}


object Network {


  /**
    * Basic constructor
    * @param nodes
    * @param links
    * @return
    */
  //def apply(nodes: Set[Node],links: Set[Link]): Network = Network(nodes=nodes,links=links)

  /**
    * additional links should be among nodes of this network ; otherwise they are added
    * FIXME reindexing with indexed network ?
    * FIXME  + node id consistency is assumed here ! do something to deactivate it/manage in a consistent way
    *
    * @param network
    * @param additionalLinks
    * @return
    */
  def apply(network: Network, additionalLinks: Set[Link]): Network = Network(
    network.nodes.union(Link.getNodes(additionalLinks)),
    network.links.union(additionalLinks)
  )

  /**
    * Same than above with an empty network
    *  - reindexed if needed
    * @param links
    * @return
    */
  def apply(links: Set[Link]): Network = {
    val nodesLinks = links.toSeq.flatMap{l=> Seq((l.e1,l),(l.e2,l))}
    if(nodesLinks.map{_._1.id}.toSet.size==nodesLinks.map{_._1}.toSet.size) return Network(nodesLinks.map{_._1}.toSet,links)
    // else need reindexing
    val indexedNodesMap: Map[Node,Node] = nodesLinks.map{_._1}.distinct.zipWithIndex.map{case (n,i) => (n,n.copy(id = i))}.toMap
    val newlinks = links.map{l=> l.copy(e1 = indexedNodesMap(l.e1), e2 = indexedNodesMap(l.e2))}
    Network(indexedNodesMap.values.toSet,newlinks)
  }


}

