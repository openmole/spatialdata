package org.openmole.spatialdata

import com.vividsolutions.jts.geom.LineString
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
import scala.util.Random



/**
  * Network classes and functions
  *
  * FIXME put graph algorithms in utils
  *
  */
package object network {

  /**
    * Node of a spatial network
    * @param id
    * @param x
    * @param y
    */
  case class Node(id: Int, position: Point2D) {
    def x = position._1
    def y = position._2
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
  case class Link(e1: Node,e2: Node,weight: Double = 1.0,length: Double = 1.0)

  object Link {
    /**
      * Link with euclidian length
      * @param e1
      * @param e2
      * @param weight
      * @return
      */
    def apply(e1: Node,e2: Node,weight: Double): Link = Link(e1,e2,weight,math.sqrt((e1.x-e2.x)*(e1.x-e2.x)+(e1.y-e2.y)*(e1.y-e2.y)))

    def getNodes(links: Set[Link]): Set[Node] = links.flatMap{l=>Set(l.e1,l.e2)}
  }

  /**
    * Network
    * @param nodes
    * @param links
    */
  case class Network(nodes: Set[Node], links: Set[Link])


  object Network {
    /**
      * additional links should be among nodes of this network ; otherwise they are added
      * @param network
      * @param additionalLinks
      * @return
      */
    def apply(network: Network, additionalLinks: Set[Link]): Network = Network(
      network.nodes.union(Link.getNodes(additionalLinks)),
      network.links.union(additionalLinks)
    )
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
  def spatialSimplification(network: Network,snap: Double): Network = empty


}
