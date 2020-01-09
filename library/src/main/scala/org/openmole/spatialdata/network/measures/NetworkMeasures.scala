package org.openmole.spatialdata.network.measures

import org.openmole.spatialdata.network._
import org.openmole.spatialdata.network.loading.ShortestPathsNetworkLoader
import org.openmole.spatialdata.network.measures.NetworkMeasures.{ShortestPathsNetworkMeasures, SummaryNetworkMeasures}
import org.openmole.spatialdata.utils.graph.GraphAlgorithms

import scala.collection.mutable
import scala.util.Random


/**
  * Network measures
  * @param measures
  * @param summary
  * @param shortestPaths
  */
case class NetworkMeasures(
                          measures: Seq[NetworkMeasures.Measures],
                          summary: Option[SummaryNetworkMeasures] = None ,
                          shortestPaths: Option[ShortestPathsNetworkMeasures] = None
                          ) {

  override def toString: String = measures.map(_.toString).mkString("\n")

}



object NetworkMeasures {

  def apply(network: Network,pathSample: Double)(implicit rng: Random): NetworkMeasures = {
    val summary = SummaryNetworkMeasures(network)
    val shortest = ShortestPathsNetworkMeasures(network, pathSample)
    NetworkMeasures(Seq(summary,shortest), Some(summary),Some(shortest))
  }

  def degreeDistribution(network: Network): Map[Node,Int] = {
    val countMap = new mutable.HashMap[Node,Int]
    //network.nodes.foreach(countMap.put(_,0)) // not needed
    network.links.foreach{l => countMap.put(l.e1,countMap.getOrElse(l.e1,0)+1); countMap.put(l.e2,countMap.getOrElse(l.e2,0)+1)}
    countMap.toMap
  }

  sealed trait Measures

  /**
    * Summary statistics
    *
    * @param nodes
    * @param links
    * @param gamma
    * @param totalLength
    * @param weakComponents
    */
  case class SummaryNetworkMeasures(
                                   nodes: Double,
                                   links: Double,
                                   gamma: Double,
                                   totalLength: Double,
                                   weakComponents: Double
                                   ) extends Measures

  object SummaryNetworkMeasures {


    def apply(network: Network): SummaryNetworkMeasures = SummaryNetworkMeasures(
      network.nodes.size.toDouble,
      network.links.size.toDouble,
      gamma(network),
      totalLength(network),
      GraphAlgorithms.connectedComponents(network).size.toDouble
    )

    /**
      * directed network density
      * @param network
      * @return
      */
    def gamma(network: Network): Double = network.links.size.toDouble / (network.nodes.size.toDouble * (network.nodes.size.toDouble - 1.0))

    def totalLength(network: Network): Double = network.links.map{_.length}.sum

  }


  /**
    * Measures linked to shortest paths
    * @param betweenness
    * @param closeness
    * @param meanPathLength
    * @param meanEfficiency
    * @param diameter
    */
  case class ShortestPathsNetworkMeasures(
                                         betweenness: Map[Link,Double],
                                         closeness: Map[Node,Double],
                                         meanPathLength: Double,
                                         meanEfficiency: Double,
                                         diameter: Double
                                         ) extends Measures {
    override def toString: String = "Mean betweenness = "+(betweenness.values.sum / betweenness.size)+"\n"+
        "Mean closeness = "+(closeness.values.sum / closeness.size)+"\n"+
        "Mean path length = "+meanPathLength+"\n"+
        "Mean efficiency = "+meanEfficiency+"\n"+
        "Diameter = "+diameter
  }

  object ShortestPathsNetworkMeasures {

    /**
      * compute network measures linked to shortest paths
      *  - closeness centrality
      *  - betweenness centrality
      * [- accessibility : node variable ?]
      *  - efficiency
      *
      * @param network
      * @return
      */
    def apply(network: Network, pathSample: Double = 1.0)(implicit rng: Random): ShortestPathsNetworkMeasures = {
      val bwloading = ShortestPathsNetworkLoader(pathSample).load(network,None)
      // FIXME quicker to use org.jgrapht.alg.scoring.BetweennessCentrality
      val nw = bwloading.loadedNetwork
      val paths = nw.cachedShortestPaths.get

      //println(paths.map{_._2._3})

      val closeness = paths.filter {
        _._2._3 > 0
      }.map { p => (p._1._1, p._1._2, 1 / p._2._3) }.groupBy(_._1).map { g => (g._1, g._2.map {
        _._3
      }.sum / g._2.size)
      }
      val diameter = paths.map {
        _._2._3
      }.max
      val meanPathLength = paths.map {
        _._2._3
      }.sum / paths.size
      val efficiencies = paths.filter {
        _._2._3 > 0
      }.map { p => p._1._1.distance(p._1._2) / p._2._3 }
      val meanEfficiency = efficiencies.sum / efficiencies.size
      ShortestPathsNetworkMeasures(
        bwloading.flows,
        closeness,
        meanPathLength,
        meanEfficiency,
        diameter
      )
    }
  }


}
