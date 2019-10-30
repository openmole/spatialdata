package org.openmole.spatialdata.network

import org.openmole.spatialdata.utils.math.GraphAlgorithms


/**
  * Network
  *
  * TODO - many functions are a mess, in particular adding nodes and links - this must be fixed and made consistent with adjacency representation
  * The sequence order may be used by functions needing it (adjacency matrix operations etc)
  *
  * TODO add directed and weighted options
  *
  * @param nodes
  * @param links
  */
case class Network(
                    nodes: Set[Node],
                    links: Set[Link],
                    cachedShortestPaths: Option[Map[(Node,Node),(Seq[Node],Seq[Link],Double)]]
                  ) {

  def hasConsistentIds: Boolean = nodes.toSeq.map(_.id).distinct.length==nodes.size

  /**
    * FIXME recompute if link weight function has changed - requires to cache the weight function
    * @param linkWeight
    * @return
    */
  def computeShortestPaths(linkWeight: Link => Double = _.weight): Network = if(cachedShortestPaths.isEmpty) this.copy(cachedShortestPaths=Some(GraphAlgorithms.shortestPaths(this,nodes.toSeq,linkWeight))) else this

}


object Network {


  def apply(nodes: Set[Node],links: Set[Link]): Network = Network(nodes,links, None)

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
    network.links.union(additionalLinks),
    None
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

