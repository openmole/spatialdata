package org.openmole.spatialdata.network

import org.openmole.spatialdata.Point2D
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
  def removeLinks(removedLinks: Set[Link],keepNodes: Boolean = false): Network = {
    if(keepNodes) this.copy(links = links.filter(!removedLinks.contains(_)),cachedShortestPaths = None)
    else {
      val keptLinks = links.filter(!removedLinks.contains(_))
      this.copy(links = keptLinks, nodes = Link.getNodes(keptLinks),cachedShortestPaths = None)
    }
  }

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
    /*def findIntersection(state: (Set[Link],Boolean)): (Set[Link],Boolean) = {
      val sremLinks = state._1.toSeq
      if(sremLinks.size<=1) return (Set.empty,true) // must stop
      val currentLink = sremLinks.head
      val nonintersectinglinks = sremLinks.tail.iterator.takeWhile(l => currentLink.intersection(l).isEmpty).toSeq
      if(nonintersectinglinks.size==sremLinks.size-1) return (sremLinks.tail.toSet,false) else return (sremLinks.tail.toSet,true)
    }
    Iterator.iterate((network.links,false))(findIntersection).takeWhile(!_._2).toSeq.last._1.nonEmpty
    */
    val inters = Link.getIntersections(links)
    inters.isEmpty
  }



  /**
    * Brutal planarization algorithm (add intersections iteratively)
    *
    * !!! FIXME !!! iterative version does not work -> get all intersections, order by link and replace links by successive sublinks
    *
    *  Other options: link filtering through embedding into topological surfaces (see PMFG etc)
    *   (removes links)
    *   Tumminello, M., Aste, T., Di Matteo, T., & Mantegna, R. N. (2005). A tool for filtering information in complex systems. Proceedings of the National Academy of Sciences, 102(30), 10421-10426.
    * @param network
    * @return
    */
  def planarize: Network = {
    /*
    def addIntersection(state: (Set[Link],Set[Link])): (Set[Link],Set[Link]) = {
      val (validLinks,remLinks) = state
      println(validLinks.size+" ; "+remLinks.size)
      val sremLinks = remLinks.toSeq
      val currentLink = sremLinks.head
      if (sremLinks.size==1) return (validLinks++Set(currentLink),Set.empty)
      //println(currentLink.intersection(sremLinks.tail.head))
      val interindex = sremLinks.tail.indexWhere(l => !currentLink.intersection(l).isEmpty)
      //println(interindex+" ; remlinks : "+remLinks.size)
      if(interindex == -1) return (validLinks++Set(currentLink),sremLinks.tail.toSet)
      val currentintersecting = sremLinks.tail(interindex)
      val intersection = currentLink.intersection(currentintersecting).get
      //println(intersection)
      // create 4 new links and put them in tail of remaining
      val (l11,l12) = (Link(currentLink.e1,intersection,currentLink.weight),Link(currentLink.e2,intersection,currentLink.weight))
      val (l21,l22) = (Link(currentintersecting.e1,intersection,currentintersecting.weight),Link(currentintersecting.e2,intersection,currentintersecting.weight))
      //println(currentLink+" inter "+currentintersecting);println("----")
      //println(l11);println(l12);println(l21);println(l22)
      //assert(l11.length>1e-3&&l12.length>1e-3&&l21.length>1e-3&&l22.length>1e-3)
      assert(l11.length>0&&l12.length>0&&l21.length>0&&l22.length>0)
      //println(l11.e1==l11.e2);println(l12.e1==l12.e2);println(l21.e1==l21.e2);println(l22.e1==l22.e2)
      (validLinks,sremLinks.tail.zipWithIndex.filter(_._2!=interindex).unzip._1.toSet++Set(
          l11,l12,l21,l22
        )
      )
    }
    Network(addIntersection(Iterator.iterate((empty.links,network.links))(addIntersection).takeWhile(_._2.size>0).toSeq.last)._1)
     */
    val inters: Seq[(Link,Node)] = Link.getIntersections(links)
    // FIXME bug if empty?
    //println(network.links.size)
    //assert(inters.size>0,s"Links: ${network.links.size}")
    if(inters.size==0){return this}

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
    // return a new network with new links - totally fucks up the ids, anyway complicated to find a mapping
    //println("total new links : "+newlinks.size)
    Network(newlinks.toSet)
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
    // FIXME note: for performance should not recompute components at each step
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

