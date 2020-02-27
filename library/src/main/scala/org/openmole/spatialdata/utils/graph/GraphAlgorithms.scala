package org.openmole.spatialdata.utils.graph

import org.jgrapht.Graph
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.alg.cycle.PatonCycleBase
import org.jgrapht.alg.shortestpath.{DijkstraShortestPath, FloydWarshallShortestPaths, JohnsonShortestPaths}
import org.jgrapht.alg.interfaces._
import org.jgrapht.graph.DefaultWeightedEdge
import org.openmole.spatialdata.network._
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.utils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

/**
  * Libraries to test:
  *   - JGraphT seems very well maintained https://github.com/jgrapht/jgrapht/
  *   - Guava https://github.com/google/guava/wiki/GraphsExplained
  *   - Apache common graph seems dead http://commons.apache.org/sandbox/commons-graph/source-repository.html
  *
  *   TODO add community detection ? NetLogo NW extension: own implementation, uses jgrapht for generators, jung for some component algos
  *
  */
object GraphAlgorithms {

  //
  // Shortest paths algorithms
  //


  sealed trait ShortestPathMethod
  case class DijkstraJGraphT() extends ShortestPathMethod
  case class FloydWarshallJGraphT() extends ShortestPathMethod
  case class JohnsonJGraphT() extends ShortestPathMethod
  //case class ScalaGraph() extends ShortestPathMethod
  case class FloydWarshall() extends ShortestPathMethod


  /**
    * Shortest paths (dispatch methods)
    * @param network network
    * @param from from nodes
    * @param to to nodes
    * @param linkWeight link weight function
    * @param method method to use
    * @return
    */
  def shortestPaths(network: Network,
                    from: Seq[Node],
                    to: Seq[Node],
                    linkWeight: Link => Double = _.weight,
                    method: ShortestPathMethod = DijkstraJGraphT()
                   ): ShortestPaths = {
    method match {
      case _ : DijkstraJGraphT => ShortestPathsAlgorithms.shortestPathsJGraphT(network, from, to, linkWeight)
      case _ : FloydWarshallJGraphT => ShortestPathsAlgorithms.allShortestPathsFloydWarshallJGraphT(network, linkWeight)
      //case _ : ScalaGraph => ShortestPathsAlgorithms.shortestPathsScalaGraph(network, vertices, linkWeight)
      case _ : FloydWarshall => ShortestPathsAlgorithms.allPairsShortestPath(network, linkWeight)
      case _ : JohnsonJGraphT => ShortestPathsAlgorithms.allShortestPathsJohnsonJGraphT(network, linkWeight)
    }
  }

  object ShortestPathsAlgorithms {

    /**
      * Generic shortest paths using different JGraphT algo
      *
      * FIXME returns empty path if no path - maybe better to not put in the return map ?
      *
      * @param network network
      * @param from from nodes
      * @param to to nodes
      * @param algorithm jgrapht algo to use
      * @param linkWeight link weight function
      * @return map of shortest paths
      */
    def shortestPathsWithJGraphTAlgorithm(network: Network,
                                          from: Seq[Node],
                                          to: Seq[Node],
                                          algorithm: Graph[Int,DefaultWeightedEdge] => ShortestPathAlgorithm[Int,DefaultWeightedEdge],
                                          linkWeight: Link => Double = _.weight
                                         ): ShortestPaths = {
      //println(s"    ${from.map(network.nodes.contains)}")
      //println(network)
      val (g,nodeMap,linkMap) = GraphConversions.networkToJGraphT(network,linkWeight)
      //println(g) // issue with graoh conversion?
      (for {
        i <- from
        j <- to
      } yield ((i,j),if(i==j) {(Seq(i),Seq.empty[Link],0.0)}
      else {
        val path = algorithm(g).getPath(i.id,j.id)
        if(path == null) {(Seq.empty[Node],Seq.empty[Link],Double.PositiveInfinity)} else {
          (path.getVertexList.asScala.map {
            nodeMap(_)
          },
            path.getEdgeList.asScala.map { e => linkMap((g.getEdgeSource(e), g.getEdgeTarget(e))) },
            path.getWeight)
        }
      })).toMap.asInstanceOf[ShortestPaths]
    }



    /**
      * Shortest paths using Dijkstra in the JGraphT library
      * @param network network
      * @param from from nodes
      * @param to to nodes
      * @param linkWeight link weight function
      * @return shortest paths
      */
    def shortestPathsJGraphT(network: Network, from: Seq[Node], to: Seq[Node], linkWeight: Link => Double = _.weight): ShortestPaths =
      shortestPathsWithJGraphTAlgorithm(network,from,to,g => new DijkstraShortestPath(g), linkWeight)

    /**
      * Floyd-Warshall of JGraphT (all pairs shortest paths)
      * @param network network
      * @param linkWeight link weight function
      * @return shortest paths
      */
    def allShortestPathsFloydWarshallJGraphT(network: Network, linkWeight: Link => Double = _.weight): ShortestPaths =
      shortestPathsWithJGraphTAlgorithm(network, network.nodes.toSeq, network.nodes.toSeq, g=> new FloydWarshallShortestPaths[Int,DefaultWeightedEdge](g),linkWeight)


    /**
      * Johnson all pairs shortest paths with JGraphT
      * @param network network
      * @param linkWeight link weight function
      * @return
      */
    def allShortestPathsJohnsonJGraphT(network: Network, linkWeight: Link => Double = _.weight): ShortestPaths =
      shortestPathsWithJGraphTAlgorithm(network, network.nodes.toSeq, network.nodes.toSeq, g=> new JohnsonShortestPaths[Int,DefaultWeightedEdge](g),linkWeight)

    /**
      * Shortest paths - by default not a spatial network as weight function is only the weight
      *
      *  -- DEPRECATED, use JGraphT instead --
      *
      * FIXME works if the network is not connected but returns existing paths only
      * FIXME sampling should not be done here
      *
      *
      */
    /*def shortestPathsScalaGraph(network: Network, vertices: Seq[Node], linkWeight: Link => Double = _.weight): ShortestPaths = {
      //println("Computing shortest paths between vertices : "+vertices)
      val (g,nodeMap,linkMap) = GraphConversions.networkToScalaGraph(network, linkWeight)
      //val odnodes = if(pathSample==1.0) vertices else Stochastic.sampleWithoutReplacementBy[Node](vertices,v => 1.0 / vertices.length.toDouble, math.floor(pathSample*vertices.length).toInt)
      val odnodes = vertices // better to do the sampling explicitly outside the function
      (for {
        i <- odnodes
        j <- odnodes
      } yield ((i,j),if(i==j) {(Seq(i),Seq.empty[Link],0.0)}
      else {
        val path = g.get(i.id).shortestPathTo(g.get(j.id))
        if(path.nonEmpty){
          (path.get.nodes.map{nodeMap(_)}.toSeq,
            path.get.edges.map{e => linkMap((e._1,e._2))}.toSeq,
            path.get.edges.map{_.weight}.sum
          )
        }
        else {(Seq.empty[Node],Seq.empty[Link],Double.PositiveInfinity)}
      })).toMap
    }*/



    /**
      * TODO: code an astar and or dynamic routing (update) for more perf of shortest paths?
      *  -> see in JGraphT
      */


    /**
      * Floid Warshall shortest paths
      *
      * See https://jgrapht.org/javadoc/org/jgrapht/alg/shortestpath/JohnsonShortestPaths.html for sparse graphs in O(n2 logN)
      *
      * - slow in O(N^3) => DO NOT USE FOR LARGE NETWORKS
      *
      * @param network
      * @return
      */
    def allPairsShortestPath(network: Network, linkWeight: Link => Double = _.weight): ShortestPaths = {
      utils.log("computing shortest paths between "+network.nodes.toSeq.size+" vertices")
      val nodenames = network.nodes.toSeq.map{_.id}
      val nodeids: Map[Int,Int] = nodenames.toSeq.zipWithIndex.toMap
      val revnodes: Map[Int,Node] = network.nodes.toSeq.zipWithIndex.map{case(n,i)=>(i,n)}.toMap
      val nodes = nodeids.keySet //not necessary, for clarity
      val mlinks = mutable.Map[Int, Set[Int]]()
      val mlinkweights = mutable.Map[(Int,Int),Double]()
      val linksMap = mutable.Map[(Int,Int),Link]()

      for(link <- network.links){
        if(!mlinks.keySet.contains(nodeids(link.e1.id))) mlinks(nodeids(link.e1.id))=Set.empty[Int]
        if(!mlinks.keySet.contains(nodeids(link.e2.id))) mlinks(nodeids(link.e2.id))=Set.empty[Int]
        // links assumed undirected in our case
        mlinks(nodeids(link.e1.id))+=nodeids(link.e2.id)
        mlinks(nodeids(link.e2.id))+=nodeids(link.e1.id)
        mlinkweights((nodeids(link.e1.id),nodeids(link.e2.id)))=linkWeight(link)
        mlinkweights((nodeids(link.e2.id),nodeids(link.e1.id)))=linkWeight(link)
        linksMap((nodeids(link.e2.id),nodeids(link.e1.id)))=link
        linksMap((nodeids(link.e1.id),nodeids(link.e2.id)))=link
      }

      val links = mlinks.toMap
      val linkweights = mlinkweights.toMap

      val n = nodes.size
      val inf = Double.MaxValue

      // Initialize distance matrix.
      val ds = Array.fill[Double](n, n)(inf)
      for (i <- 0 until n) ds(i)(i) = 0.0
      for (i <- links.keys) {
        for (j <- links(i)) {
          ds(i)(j) = linkweights((i,j))
        }
      }

      // Initialize next vertex matrix
      // O(N^3)
      val ns = Array.fill[Int](n, n)(-1)
      for (k <- 0 until n; i <- 0 until n; j <- 0 until n)
        if (ds(i)(k) != inf && ds(k)(j) != inf && ds(i)(k) + ds(k)(j) < ds(i)(j)) {
          ds(i)(j) = ds(i)(k) + ds(k)(j)
          ns(i)(j) = k
        }


      // Helper function to carve out paths from the next vertex matrix.
      def extractPath(path: ArrayBuffer[Node],pathLinks: ArrayBuffer[Link], i: Int, j: Int): Unit = {
        if (ds(i)(j) == inf) return
        val k = ns(i)(j)
        if (k != -1) {
          extractPath(path,pathLinks, i, k)
          path.append(revnodes(k))
          extractPath(path,pathLinks, k, j)
        }else {
          // otherwise k is the next node, can add the link
          assert(linksMap.contains(revnodes(i).id,revnodes(j).id),"error : "+network.links.filter{case l => l.e1.id==revnodes(i).id&&l.e2.id==revnodes(j).id}+" - "+network.links.filter{case l => l.e2.id==revnodes(i).id&&l.e1.id==revnodes(j).id})
          pathLinks.append(linksMap(revnodes(i).id,revnodes(j).id))
        }
      }

      // Extract paths.
      //val pss = mutable.Map[Int, Map[Int, Seq[Int]]]()
      val paths = mutable.Map[(Node,Node),(Seq[Node],Seq[Link],Double)]()
      for (i <- 0 until n) {
        //val ps = mutable.Map[Int, Seq[Int]]()
        for (j <- 0 until n) {
          if (ds(i)(j) != inf) {
            //val p = new ArrayBuffer[Int]()
            val currentPath = new ArrayBuffer[Node]()
            val currentPathLinks = new ArrayBuffer[Link]()
            currentPath.append(revnodes(i))
            if (i != j) {
              extractPath(currentPath, currentPathLinks, i, j)
              currentPath.append(revnodes(j))
            }
            paths((revnodes(i), revnodes(j))) = (currentPath.toSeq,currentPathLinks.toSeq,currentPathLinks.map{_.weight}.sum)
          }
        }
      }

      paths.toMap
    }



  }



  sealed trait ComponentsMethod
  case class ConnectedComponentsTraverse() extends ComponentsMethod
  case class ConnectedComponentsJGraphT() extends ComponentsMethod


  /**
    * Get connected components (undirected network)
    * @param network
    * @param method
    * @return
    */
  def connectedComponents(network: Network, method: ComponentsMethod = ConnectedComponentsTraverse()): Seq[Network] =
    method match {
      case _ : ConnectedComponentsTraverse => ComponentsAlgorithms.connectedComponentsTraverse(network)
      case _ : ConnectedComponentsJGraphT => ComponentsAlgorithms.connectedComponentsJGraphT(network)
    }



  /**
    * get largest connected component
    *
    * @param network
    * @return
    */
  def largestConnectedComponent(network: Network, method: ComponentsMethod = ConnectedComponentsTraverse()): Network = {
    val components = connectedComponents(network, method)
    val largestComp = components.sortWith { case (n1, n2) => n1.nodes.size > n2.nodes.size }(0)
    largestComp
  }



  object ComponentsAlgorithms {

    /**
      * Undirected weak components using JGraphT
      *
      * FIXME untested - yield empty components
      *
      * @param network
      * @return
      */
    def connectedComponentsJGraphT(network: Network): Seq[Network] = {
      val (g,nodeMap,_) = GraphConversions.networkToJGraphT(network)
      new ConnectivityInspector(g).connectedSets().asScala.map{ nodeindices => network.subNetworkNodes(nodeindices.asScala.map(nodeMap(_)).toSet)}.toSeq
    }


    /**
      * dirty component traverser (not appropriate network data structure)
      *
      * FIXME remove the mutable and the while
      *
      * @param network
      * @return
      */
    def connectedComponentsTraverse(network: Network): Seq[Network] = {
      //utils.log(s"Traversing graph of size (${network.nodes.size};${network.links.size}) for connected components")

      val nlinks = new mutable.HashMap[Node, Seq[Link]]()
      network.links.foreach { l =>
        if (nlinks.contains(l.e1)) {
          nlinks(l.e1) = nlinks(l.e1) ++ Seq(l)
        } else {
          nlinks(l.e1) = Seq(l)
        }
        if (nlinks.contains(l.e2)) {
          nlinks(l.e2) = nlinks(l.e2) ++ Seq(l)
        } else {
          nlinks(l.e2) = Seq(l)
        }
      }
      network.nodes.foreach { n => if (!nlinks.contains(n)) {
        nlinks(n) = Seq.empty
      }
      }

      //traverse using the map, using hash consing
      val totraverse = new mutable.HashMap[Node, Node]()
      network.nodes.foreach { n => totraverse.put(n, n) }
      val res = new ArrayBuffer[Network]()

      def otherend(n: Node, l: Link): Node = {
        if (l.e1 == n) l.e2 else l.e1
      }

      def traversenode(n: Node): (Seq[Node], Seq[Link]) = {
        if (!totraverse.contains(n)) {
          return (Seq.empty, nlinks(n))
        } // note : a bit redundancy on links here as they are not colored
        totraverse.remove(n)
        val traversed = nlinks(n).map { l => traversenode(otherend(n, l)) }
        (Seq(n) ++ traversed.flatMap(_._1), traversed.flatMap(_._2))
      }

      while (totraverse.nonEmpty) {
        val entry = totraverse.values.head
        val currentcomponent = traversenode(entry)
        res.append(Network(currentcomponent._1.toSet, currentcomponent._2.toSet))
      }

      res.toSeq
    }


  }



  sealed trait CycleDetectionMethod
  case class PatonJGraphT() extends CycleDetectionMethod


  def cycles(network: Network, method: CycleDetectionMethod = PatonJGraphT()): Seq[Network] =
    method match {
      case _ : PatonJGraphT => CycleAlgorithms.cyclesPatonJGraphT(network)
    }

  object CycleAlgorithms {

    /**
      * FIXME returned graphs have consistent ids but no properties of initial graph (should copy ?)
      * @param nw
      * @return
      */
    def cyclesPatonJGraphT(nw: Network): Seq[Network] = {
      val (g,nodeMap,edgeMap) = GraphConversions.networkToJGraphT(nw)
      val cycles = new PatonCycleBase(g).getCycleBasis.getCycles.asScala.toSeq
      cycles.map{l =>
        val edges = l.asScala.map{e => edgeMap((g.getEdgeSource(e),g.getEdgeTarget(e)))}.toSet
        Network(Link.getNodes(edges),edges)
      }
    }

  }



  object SimplificationAlgorithm {


    /**
      * Remove nodes with degree equal to two
      * ! fails if network has self-loops
      * implemented iteratively with mutable Sets for performance
      *
      * @param network
      * @param combineLength function to combine lengths
      * @return
      */
    def simplifyNetwork(network: Network,
                        combineLength: (Link,Link)=>Double = {case (l1,l2)=>l1.length+l2.length},
                        combineWeights: (Link,Link)=>Double = {case (l1,l2)=>(l1.weight*l1.length + l2.weight*l2.length)/(l1.length+l2.length)}
                       ): Network = {
      val nodes = mutable.HashSet.from(network.nodes)
      val links = mutable.HashSet.from(network.links)
      val nodeLinkMap = new mutable.HashMap[Node,Set[Link]]
      links.foreach{l => nodeLinkMap.put(l.e1,nodeLinkMap.getOrElse(l.e1,Set.empty[Link])++Set(l)); nodeLinkMap.put(l.e2,nodeLinkMap.getOrElse(l.e2,Set.empty[Link])++Set(l))}
      val degrees = mutable.HashMap.from(NetworkMeasures.degreeDistribution(network)) // note: could not recompute degree, ~ same
      while(degrees.values.count(_==2)>0){
        // not performant to do a toSeq?
        val toremove = degrees.toSeq.filter(_._2==2).head._1
        nodes.remove(toremove)
        //println("degree="+degrees(toremove))
        degrees.remove(toremove)
        val replacedlinks = nodeLinkMap(toremove).toSeq
        //println("toremove : "+toremove)
        //println("replaced links : "+replacedlinks)
        nodeLinkMap.remove(toremove)
        replacedlinks.foreach{l =>
          links.remove(l)
          nodeLinkMap.put(l.e1,nodeLinkMap.getOrElse(l.e1,Set(l))--Set(l))
          nodeLinkMap.put(l.e2,nodeLinkMap.getOrElse(l.e2,Set(l))--Set(l))
        }

        val othernodes = replacedlinks.flatMap(l => Set(l.e1,l.e2)).filter(_!=toremove)
        //println("othernodes : "+othernodes)
        // this will fail if there are self loops
        //assert(othernodes.size==2,"In network simplification: removed vertice had not two neighbors")
        val newlink = Link(othernodes(0),othernodes(1),combineWeights(replacedlinks(0),replacedlinks(1)),combineLength(replacedlinks(0),replacedlinks(1)),false)
        links.add(newlink)
        //println("new link : "+newlink+"\n")
        assert(othernodes.size==2,"In network simplification: removed vertice had not two neighbors")

        nodeLinkMap.put(othernodes(0),nodeLinkMap.getOrElse(othernodes(0),Set.empty[Link])++Set(newlink))
        nodeLinkMap.put(othernodes(1),nodeLinkMap.getOrElse(othernodes(1),Set.empty[Link])++Set(newlink))
        // no need to update the degree
      }
      Network(nodes.toSet,links.toSet)
    }


  }




}
