package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.network.{Link, Network, NetworkGenerator, Node}

import scala.util.Random

/**
  * A generic generator for the heuristic: iteratively, add nodes, add links.
  *  (cf Multi-modeling approach)
  */
case class NodesLinksAdditionGenerator(
                                      initialNetwork: NetworkGenerator,
                                      nodesAdditionHeuristics: Seq[Network => Set[Node]],
                                      linksAdditionHeuristics: Seq[Network => Set[Link]],
                                      stoppingCondition: Network => Boolean // rq: may trick and add network age for a fixed number of iterations e.g.
                                      ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = {
    Iterator.iterate(initialNetwork.generateNetwork){
      n: Network =>
        linksAdditionHeuristics.foldLeft(
          nodesAdditionHeuristics.foldLeft(n){case (nn,fun) => nn.addNodes(fun(nn))}
        ){
          case (nn,fun) => nn.addLinks(fun(nn))
        }
    }.takeWhile(stoppingCondition).toSeq.last
  }

}
