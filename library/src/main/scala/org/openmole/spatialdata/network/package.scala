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

  type ShortestPaths = Map[(Node,Node),(Seq[Node],Seq[Link],Double)]

  implicit class ShortestPathsDecorator(sp1: ShortestPaths){
    def |-|(sp2: ShortestPaths): Double =
      sp1.keySet.intersect(sp2.keySet).map{k => math.abs(sp1(k)._3 - sp2(k)._3)}.sum // path sets are assumed to be between same nodes
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
    val inters: Seq[(Link,Node)] = getIntersections(network.links)
    // FIXME bug if empty?
    //println(network.links.size)
    //assert(inters.size>0,s"Links: ${network.links.size}")
    if(inters.size==0){return network}

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
    * Compute intersections of a set of links
    * @param links
    * @return
    */
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
    //println("intersections (isPlanar) : "+inters.size)
    inters.isEmpty
  }



}
