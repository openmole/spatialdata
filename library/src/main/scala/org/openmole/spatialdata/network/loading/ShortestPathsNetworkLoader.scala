package org.openmole.spatialdata.network.loading

import org.openmole.spatialdata.network.{Link, Network, Node}

import scala.util.Random

case class ShortestPathsNetworkLoader(
                                       network: Network,
                                       pathSample: Double = 1.0
                                     ) extends NetworkLoader {

  override def load(odPattern: Option[Map[(Node, Node), Double]])(implicit rng: Random): NetworkLoading =
    ShortestPathsNetworkLoader.load(network, odPattern,pathSample)

}


object ShortestPathsNetworkLoader {


  def load(network: Network, odPattern: Option[Map[(Node, Node), Double]], pathSample: Double)(implicit rng: Random): NetworkLoading = {
    //  this requires that network nodes have ids ! => ok checked in shortest paths
    //val paths: Map[(Node,Node),(Seq[Node],Seq[Link],Double)] = Graph.shortestPaths(network,network.nodes.toSeq)
    val nwWithPaths = network.computeShortestPaths(l=> l.weight*l.length,pathSample)
    val paths = nwWithPaths.cachedShortestPaths.get
    val odflows: Seq[(Seq[Link],Double)] = odPattern match {
      case None => paths.keySet.map{k => (paths(k)._2,1.0)}.toSeq
      case Some(s) => paths.keySet.map{k => (paths(k)._2,s.getOrElse(k,1.0))}.toSeq
    }
    // build loaded link set
    def loadLinks(state: (Map[Link,Double],Seq[(Seq[Link],Double)])): (Map[Link,Double],Seq[(Seq[Link],Double)])= {
      val (previousLinks,paths) = state
      if(paths.isEmpty) return (previousLinks,Seq.empty)
      val (currentPath,flow) = paths.head
      val updatedLinks = currentPath.map{l => (l,previousLinks.getOrElse(l,0.0) + flow)}
      (previousLinks.filter{case (l,_) => !updatedLinks.map{_._1}.contains(l)}++updatedLinks,paths.tail)
    }
    val loading = Iterator.iterate((Map.empty[Link,Double],odflows))(loadLinks).takeWhile(_._2.size>0).toSeq.last
    val finalLoading = loadLinks(loading)._1.map{case (l,d)=>(l,d / odflows.length)} // normalize by number of paths
    NetworkLoading(nwWithPaths,finalLoading,odPattern)
  }

}
