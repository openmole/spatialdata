
package spatialdata.network

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scalax.collection.{Graph, GraphEdge}
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.GraphTraversal._
import scalax.collection.edge.WUnDiEdge
import spatialdata._

case class Node(id: Int, x: Double, y: Double)

case class Link(e1: Node,e2: Node,weight: Double = 1.0,length: Double = 1.0)

object Link {def apply(e1: Node,e2: Node,weight: Double): Link = Link(e1,e2,weight,math.sqrt((e1.x-e2.x)*(e1.x-e2.x)+(e1.y-e2.y)*(e1.y-e2.y)))}

case class Network(nodes: Set[Node], links: Set[Link])


/**
  * Network functions
  */
object Network {

  def empty: Network = Network(Set.empty,Set.empty)

  /**
    * percolate each potential link with a zero proba
    * @param network
    * @return
    */
  def percolate(network: Network,percolationProba: Double,linkFilter: Link=>Boolean//={_.weight==0.0}
               )(implicit rng: Random): Network = {
    //println("percolate : "+network.links.toSeq.map(_.weight).sum)
    val emptyLinks = network.links.toSeq.filter(linkFilter)
    val fullLinks = network.links.toSeq.filter{l => !linkFilter(l)}

    //println("percolating nw with "+emptyLinks.size+" empty links ; "+fullLinks.size+" full links")

    val percolated = emptyLinks.map{case l => if(rng.nextDouble()<percolationProba){Link(l.e1,l.e2,1.0)}else{Link(l.e1,l.e2,0.0)}}
    //println("rem empty links : "+percolated.filter((_.weight==0.0)).size)
    val newlinks=fullLinks++percolated
    //println("percolated : "+(network.links.toSeq.map(_.weight).sum+newlinks.map(_.weight).sum))
    val newLinksSet = newlinks.toSet
    //println("perc nw links = "+newLinksSet.size)
    //println("perc nw unique links = "+newLinksSet.map{case e => e.e1.id+"-"+e.e2.id+"-"+e.weight}.size)
    Network(network.nodes,newLinksSet)
  }


  /**
    * network to grid
    * @param network
    * @return
    */
  def networkToGrid(network: Network,footPrintResolution: Double = 1.0,linkwidth: Double = 1.0): RasterLayerData[Double] = {
    val xmin = network.nodes.map{_.x}.min;val xmax = network.nodes.map{_.x}.max
    val ymin = network.nodes.map{_.y}.min;val ymax = network.nodes.map{_.y}.max
    def xcor(x: Double): Int = math.max(xmin.toDouble,math.min(xmax.toDouble,math.round(x))).toInt
    def ycor(y: Double): Int = math.max(ymin.toDouble,math.min(ymax.toDouble,math.round(y))).toInt
    val res: Array[Array[Double]] = (xmin to xmax by 1.0).toArray.map{case _ => (ymin to ymax by 1.0).toArray.map{case _ =>0.0}}
    network.links.toSeq.filter{_.weight>0.0}.foreach{case l =>
      val i1 = l.e1.x - xmin;val j1 = l.e1.y - ymin
      val i2 = l.e2.x - xmin;val j2 = l.e2.y - ymin
      val istep = (i1 - i2) match {case x if math.abs(x) < 1e-10 => 0.0 ;case _ => math.cos(math.atan((j2 - j1)/(i2 - i1)))*footPrintResolution}
      val jstep = (j1 - j2) match {case x if math.abs(x) < 1e-10 => 0.0 ;case _ => math.sin(math.atan((j2 - j1)/(i2 - i1)))*footPrintResolution}
      //println("istep,jstep = "+istep+","+jstep)
      //println(l)
      val nsteps = (i1 - i2) match {case x if math.abs(x) < 1e-10 => (j2 - j1)/jstep;case _ => (i2 - i1)/istep}
      //println(nsteps)
      var x = l.e1.x;var y = l.e1.y
      (0.0 to nsteps by 1.0).foreach{_ =>
        //println("x = "+x+"; y = "+y)
        /*( - (linkwidth-1)/2 to (linkwidth-1)/2 by 1.0).toSeq.zip(( - (linkwidth-1)/2 to (linkwidth-1)/2 by 1.0)).foreach {
          case (k1,k2) =>
            res(xcor(x+k1))(ycor(y+k2)) = 1.0
        }*/
        for {
          k1 <- - (linkwidth-1)/2 to (linkwidth-1)/2 by 1.0
          k2 <-  - (linkwidth-1)/2 to (linkwidth-1)/2 by 1.0
        } yield {
          res(xcor(x+k1))(ycor(y+k2)) = 1.0
        }

        x = x + istep;y = y+ jstep
      }
    }
    res
  }


  /**
    * Reconstruct a network from the matrix representation of the world
    * (level of the patch, different from the generating network in the case of percolation)
    *  - put links in both senses
    * @param world
    * @return
    */
  def gridToNetwork(world: Array[Array[Double]]): Network = {
    val nodes = new ArrayBuffer[Node]()
    val links = new ArrayBuffer[Link]()
    var nodeid = 0
    for(i <- 0 until world.size; j <- 0 until world(0).size) {
      if(world(i)(j)>0.0){
        val currentnode = Node(nodeid,i,j);nodeid=nodeid+1
        if(i-1>0){if(world(i-1)(j)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i-1,j)))}}
        if(i+1<world.size){if(world(i+1)(j)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i+1,j)))}}
        if(j-1>0){if(world(i)(j-1)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i,j-1)))}}
        if(j+1<world(0).size){if(world(i)(j+1)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i,j+1)))}}
      }
    }
    Network(links.map{_.e1}.toSet.union(links.map{_.e2}.toSet),links.toSet)
  }




  /**
    * convert a Network to a Graph object
    * @param network
    * @return
    */
  def networkToGraph(network: Network): (Graph[Int,WUnDiEdge],Map[Int,Node]) = {
    //var linklist = ArrayBuffer[WUnDiEdge[Int]]()
    //for(link <- network.links){linklist.append()}
    //println("links = "+network.links.toSeq.size)
    val linkset = network.links.toSeq.map{case link => link.e1.id~link.e2.id % link.weight}
    //println("linkset = "+linkset.size)
    (Graph.from(linkset.flatten,linkset.toList),network.nodes.map{(n:Node)=>(n.id,n)}.toMap)
  }

  /**
    *
    * @param graph
    * @return
    */

  def graphToNetwork(graph: Graph[Int,WUnDiEdge],nodeMap: Map[Int,Node]): Network = {
    val links = ArrayBuffer[Link]();val nodes = ArrayBuffer[Node]()
    for(edge <-graph.edges){
      //links.append(Link(edge._1,edge._2,edge.weight))
      nodes.append(nodeMap(edge._1),nodeMap(edge._2))
      links.append(Link(nodeMap(edge._1),nodeMap(edge._2),edge.weight))
    }
    Network(nodes.toSet,links.toSet)
  }


  /**
    *
    * @param network
    * @param vertices
    * @return
    */
  def shortestPathsScalagraph(network: Network, vertices: Seq[Node]): Map[(Node,Node),(Seq[Node],Double)] = {
    //println("Computing shortest paths between vertices : "+vertices)
    val gg = networkToGraph(network)
    val g = gg._1
    val nodeMap = gg._2
    (for {
      i <- vertices
      j <- vertices
    } yield ((i,j),if(i==j) {(Seq(i),0.0)}
      else {
        val path = g.get(i.id).shortestPathTo(g.get(j.id))
        if(path.nonEmpty){
          (path.get.nodes.map{nodeMap(_)}.toSeq,path.get.edges.map{_.weight}.sum)
        }
        else {(Seq.empty[Node],Double.PositiveInfinity)}
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
    * @param network
    * @return
    */
  def connectedComponents(network: Network): Seq[Network] = {
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
    * Floid marshall shortest paths
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


