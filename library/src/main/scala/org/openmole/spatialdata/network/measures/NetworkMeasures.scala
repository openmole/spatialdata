package org.openmole.spatialdata.network.measures

import org.openmole.spatialdata.network._
import org.openmole.spatialdata.network.loading.ShortestPathsNetworkLoader

import scala.util.Random




case class NetworkMeasures(
                          measures: Seq[NetworkMeasures.Measures]
                          ) {

  override def toString: String = measures.map(_.toString).mkString("\n")

}



object NetworkMeasures {

  def apply(network: Network)(implicit rng: Random): NetworkMeasures = NetworkMeasures(Seq(SummaryNetworkMeasures(network),ShortestPathsNetworkMeasures(network)))

  sealed trait Measures

  case class SummaryNetworkMeasures(
                                     gamma: Double
                                   ) extends Measures

  object SummaryNetworkMeasures {


    def apply(network: Network): SummaryNetworkMeasures = SummaryNetworkMeasures(gamma(network))

    /**
      * directed network density
      * @param network
      * @return
      */
    def gamma(network: Network): Double = network.links.size.toDouble / (network.nodes.size.toDouble * (network.nodes.size.toDouble - 1.0))


  }


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
      val bwloading = ShortestPathsNetworkLoader(network, pathSample).load(None)
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
