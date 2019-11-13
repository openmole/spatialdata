package org.openmole.spatialdata.utils.graph

import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.openmole.spatialdata.network._
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Stochastic

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import collection.JavaConverters._

/**
  * Libraries to test:
  *   - JGraphT seems very well maintained https://github.com/jgrapht/jgrapht/
  *   - Guava https://github.com/google/guava/wiki/GraphsExplained
  *   - Apache common graph seems dead http://commons.apache.org/sandbox/commons-graph/source-repository.html
  */
object GraphAlgorithms {


  /**
    * Shortest paths using Dijkstra in the JGraphT library
    * @param network
    * @param vertices
    * @param linkWeight
    * @param pathSample
    * @param rng
    * @return
    */
  def shortestPathsJGraphT(network: Network, vertices: Seq[Node], linkWeight: Link => Double = _.weight,pathSample: Double = 1.0)(implicit rng: Random): ShortestPaths = {
    val (g,nodeMap,linkMap) = GraphConversions.networkToJGraphT(network,linkWeight)
    val algo = new DijkstraShortestPath(g)
    (for {
      i <- vertices
      j <- vertices
    } yield ((i,j),if(i==j) {(Seq(i),Seq.empty[Link],0.0)}
    else {
      val path = algo.getPath(i.id,j.id)
      (path.getVertexList.asScala.map{nodeMap(_)},
        path.getEdgeList.asScala.map{e => linkMap((g.getEdgeSource(e),g.getEdgeTarget(e)))},
      path.getWeight)
    })).toMap
  }


  /**
    * Shortest paths - by default not a spatial network as weight function is only the weight
    *
    * FIXME works if the network is not connected but returns existing paths only
    * FIXME sampling should not be done here
    *
    * @param network
    * @param vertices
    * @return
    */
  def shortestPaths(network: Network, vertices: Seq[Node], linkWeight: Link => Double = _.weight,pathSample: Double = 1.0)(implicit rng: Random): ShortestPaths = {
    //println("Computing shortest paths between vertices : "+vertices)
    val (g,nodeMap,linkMap) = GraphConversions.networkToGraph(network, linkWeight)
    val odnodes = if(pathSample==1.0) vertices else Stochastic.sampleWithoutReplacementBy[Node](vertices,v => 1.0 / vertices.length.toDouble, math.floor(pathSample*vertices.length).toInt)
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
  }



  /**
    * extract connected components
    *  using scala-graph component traverser
    *
    *
    *
    * @param network
    * @return
    */
  // FIXME does not work with latest scala-graph version
  /*def connectedComponentsScalagraph(network: Network): Seq[Network] = {
    val (graph,nodeMap) = networkToGraph(network)
    //val components: Seq[graph.Component] = graph.componentTraverser().toSeq
    val components: Seq[graph.Component] = (for (component <- graph.componentTraverser()) yield component).toSeq
    //println("components : "+components.size)
    components.map{case c => graphToNetwork(Graph.from(c.nodes,c.edges),nodeMap)}

  }*/

  /**
    * dirty component traverser (not appropriate network data structure)
    *
    * FIXME remove the mutable and the while
    *
    * @param network
    * @return
    */
  def connectedComponents(network: Network): Seq[Network] = {
    utils.log(s"Traversing graph of size (${network.nodes.size};${network.links.size}) for connected components")
    val nlinks = new mutable.HashMap[Node,Seq[Link]]()
    network.links.foreach{l =>
      if(nlinks.contains(l.e1)){nlinks(l.e1)=nlinks(l.e1)++Seq(l)}else{nlinks(l.e1)=Seq(l)}
      if(nlinks.contains(l.e2)){nlinks(l.e2)=nlinks(l.e2)++Seq(l)}else{nlinks(l.e2)=Seq(l)}
    }
    network.nodes.foreach{n=> if(!nlinks.contains(n)){nlinks(n)=Seq.empty}}

    //traverse using the map, using hash consing
    val totraverse = new mutable.HashMap[Node,Node]()
    network.nodes.foreach{n=>totraverse.put(n,n)}
    val res = new ArrayBuffer[Network]()

    def otherend(n:Node,l:Link):Node = {if(l.e1==n)l.e2 else l.e1}

    def traversenode(n: Node): (Seq[Node],Seq[Link]) = {
      if(!totraverse.contains(n)){return((Seq.empty,nlinks(n)))} // note : a bit redundancy on links here as they are not colored
      totraverse.remove(n)
      val traversed = nlinks(n).map{l => traversenode(otherend(n,l))}
      (Seq(n)++traversed.map{_._1}.flatten,traversed.map{_._2}.flatten)
    }

    while(totraverse.size>0){
      val entry = totraverse.values.head
      val currentcomponent = traversenode(entry)
      res.append(Network(currentcomponent._1.toSet,currentcomponent._2.toSet))
    }

    res
  }

  /**
    * get largest connected component
    * @param network
    * @return
    */
  def largestConnectedComponent(network: Network): Network = {
    val components = connectedComponents(network)
    //val largestComp = components.sortWith{case(n1,n2)=>n1.nodes.size>=n2.nodes.size}(0)
    val largestComp = components.sortWith{case(n1,n2)=>n1.nodes.size>n2.nodes.size}(0)
    //println("largest comp size : "+largestComp.nodes.size)
    largestComp
  }





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
  def allPairsShortestPath(network: Network): Map[(Node,Node),Seq[Link]] = {
    println("computing shortest paths between "+network.nodes.toSeq.size+" vertices")
    val nodenames = network.nodes.toSeq.map{_.id}
    println("unique nodes id = "+nodenames.size)
    val nodeids: Map[Int,Int] = nodenames.toSeq.zipWithIndex.toMap
    //val revnodeids: Map[Int,Int] = nodenames.zipWithIndex.map{case(oid,ind)=>(ind,oid)}.toMap
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
      mlinkweights((nodeids(link.e1.id),nodeids(link.e2.id)))=link.weight
      mlinkweights((nodeids(link.e2.id),nodeids(link.e1.id)))=link.weight
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

    println(ds.flatten.filter(_!=inf).size)
    println(2*network.links.size+network.nodes.size)

    // Initialize next vertex matrix
    // O(N^3)
    val ns = Array.fill[Int](n, n)(-1)
    for (k <- 0 until n; i <- 0 until n; j <- 0 until n)
      if (ds(i)(k) != inf && ds(k)(j) != inf && ds(i)(k) + ds(k)(j) < ds(i)(j)) {
        ds(i)(j) = ds(i)(k) + ds(k)(j)
        ns(i)(j) = k
      }

    // FIX for unconnected networks ? should also work as ds(i)(j) = inf ?
    println(ns.flatten.filter(_==(-1)).size)
    println(network.links.size)


    // Helper function to carve out paths from the next vertex matrix.
    def extractPath(path: ArrayBuffer[Node],pathLinks: ArrayBuffer[Link], i: Int, j: Int) {
      if (ds(i)(j) == inf) return
      val k = ns(i)(j)
      if (k != -1) {
        extractPath(path,pathLinks, i, k)
        //assert(revnodes.contains(j),"error : "+k)
        path.append(revnodes(k))
        extractPath(path,pathLinks, k, j)
      }else {
        // otherwise k is the next node, can add the link
        //assert(revnodes.contains(i),"error : "+i)
        //assert(revnodes.contains(j),"error : "+j)
        assert(linksMap.contains(revnodes(i).id,revnodes(j).id),"error : "+network.links.filter{case l => l.e1.id==revnodes(i).id&&l.e2.id==revnodes(j).id}+" - "+network.links.filter{case l => l.e2.id==revnodes(i).id&&l.e1.id==revnodes(j).id})
        pathLinks.append(linksMap(revnodes(i).id,revnodes(j).id))
      }
    }

    // Extract paths.
    //val pss = mutable.Map[Int, Map[Int, Seq[Int]]]()
    val paths = mutable.Map[(Node,Node),Seq[Link]]()
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
          paths((revnodes(i), revnodes(j))) = currentPathLinks.toSeq
        }
      }
    }

    paths.toMap
  }



}
