package org.openmole.spatialdata

import org.locationtech.jts.geom.LineString
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scalax.collection.{Graph, GraphEdge}
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.GraphTraversal._
import scalax.collection.edge.WUnDiEdge
import org.openmole.spatialdata._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scala.util.Random

import org.openmole.spatialdata.utils.math._


/**
  * Network classes and functions
  *
  * FIXME put graph algorithms in utils
  *
  */
package object network {

  /**
    * Node of a spatial network
    * @param id // note: how to impose comparison only on positions? -> all ids to 0?
    * @param position point in a 2D space
    */
  case class Node(id: Int, position: Point2D) {
    def x: Double = position._1
    def y: Double = position._2

    def distance(n2: Node): Double = math.sqrt(math.pow(x-n2.x,2)+math.pow(y-n2.y,2))

    /**
      * lexicographic order
      * @param n2
      * @return
      */
    def <=(n2: Node): Boolean = x<n2.x||(x==n2.x&&y<=n2.y)

  }

  object Node {
    def apply(id: Int,x: Double,y: Double): Node = Node(id,(x,y))
  }

  /**
    * Link of a spatial network
    * @param e1
    * @param e2
    * @param weight
    * @param length
    */
  case class Link(e1: Node,e2: Node,weight: Double,length: Double) {

    def x: Double = (e1.x + e2.x)/2
    def y: Double = (e1.y + e2.y)/2

    def heading: Double = (e1.x - e2.x,e1.y - e2.y) match {
      case (0.0,0.0) => 0.0 // by convention
      case (0.0,dy) => math.Pi/2*dy/math.abs(dy)
      case (dx,dy) => math.atan(dy/dx)
    }

    /**
      * !!! prev error: sin <-> cos (complementary def of heading)
      * @param xx
      * @return
      */
    def xwithin(xx: Double): Boolean = math.abs(x - xx) <= math.abs(length / 2 * math.cos(heading))
    def ywithin(yy: Double): Boolean = math.abs(y - yy) <= math.abs(length / 2 * math.sin(heading))

    /**
      * Compute intersection with an other link
      * @param l2
      * @return
      */
    def intersection(l2: Link): Option[Node] = {
      //println("inter? "+this+" ; "+l2)
      if(this.length==0.0||l2.length==0.0) return None
      if(e1==e2||l2.e1==l2.e2||e1==l2.e1||e1==l2.e2||e2==l2.e1||e2==l2.e2) return None // by convention no intersection if one common extremity
      val (h1,h2) = (heading,l2.heading)
      if(math.abs(h1)==math.Pi/2&&math.abs(h2)==math.Pi/2) return None
      val (m1,m2) = (math.tan(h1),math.tan(h2))
      if(m1 == m2) return None // parallel links
      if(math.abs(h1)==math.Pi/2){// this vertical link
        val c2 = l2.y - l2.x * m2
        val xx = this.x
        val yy = m2 * xx + c2
        if((!l2.xwithin(xx))||(!this.ywithin(yy))) return None else {
          //val inter = Node(0,digits(xx,4),digits(yy,4))
          val inter = Node(0,xx,yy)
          if (e1==inter||e2==inter||l2.e1==inter||l2.e2==inter) return None
          else return Some(inter)
        }
      }
      if(math.abs(h2)==math.Pi/2){ // vertical second link
        val c1 = this.y - this.x * m1
        val xx = l2.x
        val yy = m1 * xx + c1
        if((!xwithin(xx))||(!l2.ywithin(yy))) return None else {
          //val inter = Node(0,digits(xx,4),digits(yy,4))
          val inter = Node(0,xx,yy)
          if (e1==inter||e2==inter||l2.e1==inter||l2.e2==inter) return None
          else return Some(inter)
        }
      }
      // intercepts
      val c1 = this.y - this.x * m1
      val c2 = l2.y - l2.x * m2
      val xx = (c2 - c1) / (m1 - m2)
      if((!xwithin(xx))||(!l2.xwithin(xx))) None else {
        //val inter = Node(0,digits(xx,4),digits(m1 * xx + c1,4))
        val inter = Node(0,xx,m1 * xx + c1)
        if (e1==inter||e2==inter||l2.e1==inter||l2.e2==inter) return None
        else return Some(inter)
      }
    }

  }

  object Link {
    /**
      * Link with euclidian length
      * @param e1
      * @param e2
      * @param weight
      * @return
      */
    def apply(e1: Node,e2: Node,weight: Double): Link = Link(e1,e2,weight,math.sqrt((e1.x-e2.x)*(e1.x-e2.x)+(e1.y-e2.y)*(e1.y-e2.y)))

    def apply(e1: Node, e2: Node):Link = apply(e1,e2,1.0)

    def apply(e1: Node, e2: Node, directed: Boolean): Link = if(directed) Link(e1,e2) else {
      if(e1<=e2) Link(e1,e2) else Link(e2,e1)
    }

    def getNodes(links: Set[Link]): Set[Node] = links.flatMap{l=>Set(l.e1,l.e2)}
  }

  /**
    * Network
    *
    * TODO - many functions are a mess, in particular adding nodes and links - this must be fixed and made consistent with adjacency representation
    * The sequence order may be used by functions needing it (adjacency matrix operations etc)
    *
    * @param nodes
    * @param links
    */
  case class Network(nodes: Set[Node], links: Set[Link])
  /*trait Network {
    def nodes: Set[Node]
    def links: Set[Link]
  } */


  object Network {
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
      * same than above with an empty network
      * @param links
      * @return
      */
    def apply(links: Set[Link]): Network = Network(Link.getNodes(links),links)

      /*new Network {
      override def nodes = network.nodes.union(Link.getNodes(additionalLinks))
      override def links = network.links.union(additionalLinks)
    }*/
  }


  /**
    * empty network
    * @return
    */
  def empty: Network = Network(Set.empty[Node],Set.empty[Link])

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
    * Network to grid: rasterize a network
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
      val nsteps = (i1 - i2) match {case x if math.abs(x) < 1e-10 => (j2 - j1)/jstep;case _ => (i2 - i1)/istep}
      var x = l.e1.x;var y = l.e1.y
      (0.0 to nsteps by 1.0).foreach{_ =>
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
    * import a network from gis linestrings
    * @param lines
    * @param snap
    * @return
    */
  def fromGISLines(lines: Seq[LineString], snap: Double): Network = {
    /*lines.zipWithIndex.map{
      case (line,i) => line.getCoordinates.zip
    }*/
    // would require some kind of spatial index for efficiency ? -> do the snapping directly here using a hashmap with rounded coordinates
    // (quite dirty)

    // TODO
    empty
  }


  /**
    * simplify a spatial network through snapping
    * @param network
    * @param snap
    * @return
    */
  def spatialSimplification(network: Network,snap: Double): Network = {
    // TODO
    empty
  }


  /**
    * Brutal planarization algorithm (add intersections iteratively)
    * FIXME how ids are handled by intersection
    *
    * !!! FIXME !!! iterative version does not work -> get all intersections, order by link and replace links by successive sublinks
    *
    *  Other options: link filtering through embedding into topological surfaces (see PMFG etc)
    *   (removes links)
    *   Tumminello, M., Aste, T., Di Matteo, T., & Mantegna, R. N. (2005). A tool for filtering information in complex systems. Proceedings of the National Academy of Sciences, 102(30), 10421-10426.
    * @param network
    * @return
    */
  def planarize(network: Network): Network = {
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
    val inters = getIntersections(network.links)
    // FIXME pb with precision of same inter
    println("min dist inters = "+(for {
      i1 <- inters.map{_._2}
      i2<- inters.map{_._2}
      if i1 != i2
      d = i1.distance(i2)
    } yield d).min)
    val newlinks: Seq[Link] = inters.groupBy(_._1).toSeq.flatMap{case (l,currentinters) => {
        val nodeseq = (Seq(l.e1,l.e2)++currentinters.map(_._2)).sortWith{case (n1,n2)=> n1 <= n2}
        nodeseq.dropRight(1).zip(nodeseq.drop(1)).map{case (n1,n2) => Link(n1,n2,false)}
      }
    }
    // return a new network with new links - totally fucks up the ids, anyway complicated to find a mapping
    println("total new links : "+newlinks.size)
    Network(newlinks.toSet)
  }

  def getIntersections(links: Set[Link]): Seq[(Link,Node)] = {
    (for {
      l1 <- links
      l2 <- links
      inter = l1.intersection(l2)
      if inter.isDefined
    } yield (l1,inter.get)).toSeq
  }

  /**
    * Test is a network is planar - in O(l*(l-1)/2) -> does not work ; brutal in O(l^2)
    * @param network
    * @return
    */
  def isPlanar(network: Network): Boolean = {
    /*def findIntersection(state: (Set[Link],Boolean)): (Set[Link],Boolean) = {
      val sremLinks = state._1.toSeq
      if(sremLinks.size<=1) return (Set.empty,true) // must stop
      val currentLink = sremLinks.head
      val nonintersectinglinks = sremLinks.tail.iterator.takeWhile(l => currentLink.intersection(l).isEmpty).toSeq
      if(nonintersectinglinks.size==sremLinks.size-1) return (sremLinks.tail.toSet,false) else return (sremLinks.tail.toSet,true)
    }
    Iterator.iterate((network.links,false))(findIntersection).takeWhile(!_._2).toSeq.last._1.nonEmpty
    */
    val inters = getIntersections(network.links)
    println("intersections (isPlanar) : "+inters.size)
    inters.size==0
  }



}
