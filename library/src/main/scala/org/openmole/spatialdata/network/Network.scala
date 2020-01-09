package org.openmole.spatialdata.network

import org.openmole.spatialdata.Point2D
import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils.Implicits._
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

  /**
    * Check if node index set is injective
    * @return
    */
  def hasConsistentIds: Boolean = nodes.toSeq.map(_.id).distinct.length==nodes.size

  /**
    * force reindexing of nodes
    * @return
    */
  def withConsistentIds: Network = {
    val newNodesMap = Link.getNodes(links).zipWithIndex.map{case (n,i)=> (n,n.copy(id = i))}.toMap
    this.copy(
      nodes = newNodesMap.values.toSet,
      links = links.map{l => l.copy(e1 = newNodesMap(l.e1),e2 = newNodesMap(l.e2))}
    )
  }

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
         )
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


  /**
    * Subnetwork from nodes
    * @param subnodes
    * @return
    */
  def subNetworkNodes(subnodes: Set[Node]): Network = {
    this.copy(
      nodes = subnodes.intersect(nodes),
      links = links.filter{l => subnodes.contains(l.e1)&&subnodes.contains(l.e2)},
      cachedShortestPaths=None
    )
  }

  /**
    * Subnetwork from links
    * @param sublinks
    * @return
    */
  def subNetworkLinks(sublinks: Set[Link]): Network = {
    val keptnodes = Link.getNodes(sublinks)
    this.copy(
      nodes = nodes.intersect(keptnodes),
      links = links.intersect(sublinks),
      cachedShortestPaths = None
    )
  }

  /**
    * add new links
    * @param newLinks
    * @return
    */
  def addLinks(newLinks: Set[Link]): Network = Network(this, newLinks)

  /**
    * add nodes
    * @param newNodes
    * @return
    */
  def addNodes(newNodes: Set[Node]): Network = this.copy(nodes = nodes.union(newNodes))


  /**
    * Remove a set of links and optionally corresponding nodes if belonging only to removed links
    *  FIXME cached shortest paths are removed by default - could implement dynamic programming shortest paths and include here
    *   (should work with link removal, only recompute path if deleted link on it - requires caching paths themselves but ok)
    * @param network
    * @param removedLinks
    * @param keepNodes
    * @return
    */
  def removeLinks(removedLinks: Set[Link], keepNodes: Boolean = false): Network = {
    if(keepNodes) this.copy(links = links.filter(!removedLinks.contains(_)),cachedShortestPaths = None)
    else {
      val keptLinks = links.filter(!removedLinks.contains(_))
      this.copy(links = keptLinks, nodes = Link.getNodes(keptLinks),cachedShortestPaths = None)
    }
  }

  /**
    * Remove a number of random links
    * @param removed
    * @param keepNodes
    * @return
    */
  def removeRandomLinks(removed: Int, keepNodes: Boolean = false)(implicit rng: Random): Network = removeLinks(links.sampleWithoutReplacement(removed).toSet, keepNodes)

  /**
    * Remove nodes - ids are unchanged as restriction of injective index is still injective
    *  can not have the option to keep links as would not have any sense
    * @param network
    * @param removedNodes
    * @return
    */
  def removeNodes(removedNodes: Set[Node]): Network =
    this.copy(nodes = nodes.filter(!removedNodes.contains(_)), links = links.filter{l => (!removedNodes.contains(l.e1))&(!removedNodes.contains(l.e2))}, cachedShortestPaths = None)

  /**
    * Remove random nodes
    * @param removed
    * @param rng
    * @return
    */
  def removeRandomNodes(removed: Int)(implicit rng: Random): Network = removeNodes(nodes.sampleWithoutReplacement(removed).toSet)


  /**
    * percolate each potential link with a zero proba
    * @param network
    * @return
    */
  def percolate(percolationProba: Double,linkFilter: Link=>Boolean= {l =>l.weight==0.0})(implicit rng: Random): Network = {
    val emptyLinks = links.toSeq.filter(linkFilter)
    val fullLinks = links.toSeq.filter{l => !linkFilter(l)}
    val percolated = emptyLinks.map{case l => if(rng.nextDouble()<percolationProba){Link(l.e1,l.e2,1.0)}else{Link(l.e1,l.e2,0.0)}}
    val newlinks=fullLinks++percolated
    val newLinksSet = newlinks.toSet
    this.copy(links = newLinksSet)
  }



  /**
    * Test is a network is planar - in O(l*(l-1)/2) -> does not work ; brutal in O(l^2)
    * @param network
    * @return
    */
  def isPlanar : Boolean = {
    val inters = Link.getIntersections(links)
    inters.isEmpty
  }



  /**
    * Brutal planarization algorithm (add intersections iteratively)

    *  Other options: link filtering through embedding into topological surfaces (see PMFG etc)
    *   (removes links)
    *   Tumminello, M., Aste, T., Di Matteo, T., & Mantegna, R. N. (2005). A tool for filtering information in complex systems. Proceedings of the National Academy of Sciences, 102(30), 10421-10426.
    * @param network
    * @return
    */
  def planarize: Network = {
    val inters: Seq[(Link,Node)] = Link.getIntersections(links)

    if(inters.isEmpty){return this.withConsistentIds}

    // need to group intersections that are virtually the same
    val toreplace: Map[Node,(Node,Double)] = (for {
      i <- 0 to inters.length - 2
      j <- (i+1) until inters.length
      d = inters(i)._2.distance(inters(j)._2)
    } yield (inters(i)._2,(inters(j)._2,d))).filter(_._2._2<1e-10).toMap
    val newinters = inters.map{case (l,n) => if(toreplace.contains(n)) (l,toreplace(n)._1) else (l,n) }
    val newlinks: Seq[Link] = newinters.groupBy(_._1).toSeq.flatMap{case (l,currentinters) => {
      val nodeseq = (Seq(l.e1,l.e2)++currentinters.map(_._2)).sortWith{case (n1,n2)=> n1 <= n2}
      nodeseq.dropRight(1).zip(nodeseq.drop(1)).map{case (n1,n2) => Link(n1,n2,false)}
    }
    }
    // return a new network with new links - need to reindex the ids
    val newlinksset = newlinks.toSet
    // keep old nodes that had no links
    val newnodes = Link.getNodes(newlinksset).union(nodes.filter(n => !inters.map(_._2).contains(n)&&(!newinters.contains(n))))
    val res = Network(newnodes,newlinksset)
    //println("----------\n"+res)
    //println(res.withConsistentIds)
    res.withConsistentIds
  }


  /**
    * connect the full network with links between closest connected components
    * @return
    */
  def weakComponentConnect: Network = {
    val distances: Map[(Node,Node),Double] = (for {
      n1 <- nodes
      n2 <- nodes
      if n1 != n2
      d = n1.distance(n2)
    } yield ((n1,n2),d)).toMap
    //  note: for performance should not recompute components at each step
    def connectClosestComponents(state: (Network,Int)): (Network,Int) = {
      val components = GraphAlgorithms.connectedComponents(state._1)
      if(components.size==1) (state._1,1)
      else {
        val (n1min, n2min, dmin) = (for {
          c1 <- components
          c2 <- components if c1 != c2
          (c1min, c2min, dmin) = (for {c1n <- c1.nodes; c2n <- c2.nodes} yield (c1n, c2n, distances((c1n, c2n)))).minBy(_._3)
        } yield (c1min, c2min, dmin)).minBy(_._3)
        (state._1.addLinks(Set(Link(n1min, n2min))),components.size)
      }
    }
    Iterator.iterate((this,-1))(connectClosestComponents).takeWhile(_._2!=1).toSeq.last._1
  }

  /**
    * Get links connecting single nodes to closest
    *   TODO option to connect each node with closest neighbor before ? (cf NetLogo implementation)
    *   FIXME cannot work returning a Seq[Link], must return a network as projection node must be created !
    *   FIXME what happens with directed links ?
    * @return
    */
  def projectionConnect: Network = {
    def addLinkToProjection(state: (Network,Int)): (Network,Int) = {
      val components = GraphAlgorithms.connectedComponents(state._1)
      val nodestoconnect = components.filter(_.nodes.size == 1)
      if(nodestoconnect.isEmpty) return (state._1,0)
      val n = nodestoconnect.head.nodes.head // should be one node only
      if (state._1.links.isEmpty) return (state._1.addLinks(Set(Link(n,nodestoconnect.tail.minBy(_.nodes.head.distance(n)).nodes.head))),nodestoconnect.size - 1)
      val (projnode,projlink) = n.projection(state._1.links) //links cannot be empty
      val newLinks = Set(
        Link(projlink.e1,projnode,projlink.weight),
        Link(projnode,projlink.e2,projlink.weight),
        Link(n,projnode,projlink.weight)
      )
      (state._1.removeLinks(Set(projlink),keepNodes = true).addLinks(newLinks),nodestoconnect.size)
    }
    Iterator.iterate((this,Int.MaxValue))(addLinkToProjection).takeWhile(_._2>0).toSeq.last._1.withConsistentIds
  }



}


object Network {

  /**
    * Network with no links from spatial points
    * @param points
    * @return
    */
  def apply(points: Seq[Point2D]): Network = Network(points.zipWithIndex.map{case (p,i) => Node(i,p)}.toSet, Set.empty[Link])


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


  /**
    * empty network
    * @return
    */
  def empty: Network = Network(Set.empty[Node],Set.empty[Link])



}

