package org.openmole.spatialdata.utils.graph

import org.openmole.spatialdata.network.{Link, Network, Node}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.edge.WUnDiEdge

import scala.collection.mutable.ArrayBuffer
import org.jgrapht._
import org.jgrapht.graph.{DefaultWeightedEdge, SimpleWeightedGraph}

object GraphConversions {

  /**
    * Note: the converted jgraph does not have spatial attributes
    * FIXME take into account directed graphs
    * @param network
    * @param linkWeight
    * @return
    */
  def networkToJGraphT(network: Network, linkWeight: Link => Double = _.weight): (org.jgrapht.Graph[Int,DefaultWeightedEdge],Map[Int,Node],Map[(Int,Int),Link]) = {
    val g = new SimpleWeightedGraph[Int,DefaultWeightedEdge](classOf[DefaultWeightedEdge]) // mutable graph !
    network.links.foreach{ l =>
      Graphs.addEdgeWithVertices(g,l.e1.id,l.e2.id,linkWeight(l))
    }
    val nodeMap = network.nodes.map{n=>(n.id,n)}.toMap
    val linkMap = network.links.map{l => ((l.e1.id,l.e2.id),l)}.toMap
    (g,nodeMap,linkMap)
  }


  /**
    * convert a Network to a Graph object
    * @param network
    * @return
    */
  def networkToGraph(network: Network, linkWeight: Link => Double = _.weight): (Graph[Int,WUnDiEdge],Map[Int,Node],Map[(Int,Int),Link]) = {
    assert(network.hasConsistentIds,"Can not convert network to graph: non injective id set")
    //var linklist = ArrayBuffer[WUnDiEdge[Int]]()
    //for(link <- network.links){linklist.append()}
    //println("links = "+network.links.toSeq.size)
    val linkset = network.links.toSeq.map{case link => link.e1.id~link.e2.id % linkWeight(link)}
    //println("linkset = "+linkset.size)
    val graph = Graph.from(linkset.flatten,linkset.toList)
    val nodeMap = network.nodes.map{(n:Node)=>(n.id,n)}.toMap
    val linkMap = network.links.map{l => ((l.e1.id,l.e2.id),l)}.toMap
    (graph,nodeMap,linkMap)
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


}
