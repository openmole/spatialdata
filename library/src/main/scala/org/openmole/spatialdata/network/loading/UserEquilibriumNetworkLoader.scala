package org.openmole.spatialdata.network.loading

import org.openmole.spatialdata.network.{Link, Network, Node}
import org.openmole.spatialdata.utils

import scala.util.Random


/**
  * Static deterministic user equilibrium
  *
  * ! this must be tested and benchmarked for convergence / performance
  *
  * @param network network
  * @param linkCostFunction link cost function
  * @param pathSample proportion of paths to sample
  */
case class UserEquilibriumNetworkLoader(
                                         network: Network,
                                         linkCostFunction: (Link, Double) => Double,
                                         descentStep: Double = 0.5,
                                         epsilon: Double = 0.05,
                                         pathSample: Double = 1.0
                                       ) extends NetworkLoader {

  override def load(network: Network, odPattern:  Option[Map[(Node, Node), Double]])(implicit rng: Random): NetworkLoading =
    UserEquilibriumNetworkLoader.load(network,linkCostFunction,odPattern,pathSample,descentStep,epsilon)

}



object UserEquilibriumNetworkLoader {


  /**
    * Compute an User Equilibrium using the Franck-Wolfe algorithm
    *   (see Perederieieva, O., Ehrgott, M., Wang, J. Y., & Raith, A. (2013, October). A computational study of traffic assignment algorithms. In Australasian Transport Research Forum, Brisbane, Australia (pp. 1-18).
    *     for description and comparison of several link-based, path-based and origin-based traffic assignment algorithms
    *     !!! a step is taken as f_(t+1) = (1 - lambda) f(t) + lambda f_{shortest}(t) (nothing happens when flows are already at the shortest path)
    * @param linkCostFunction link cost function
    * @param odPattern optional od pattern
    * @param rng rng
    * @return
    */
  def load(network: Network,
           linkCostFunction: (Link,Double) => Double,
           odPattern: Option[Map[(Node,Node),Double]],
           pathSample: Double,
           descentStep: Double,
           epsilon: Double
          )(implicit rng: Random): NetworkLoading = {
    // In practice can be implemented with any other loader (not sure it converges though ?)
    val loader: NetworkLoader = ShortestPathsNetworkLoader(pathSample)
    def step(state: (NetworkLoading,Double)): (NetworkLoading,Double) = {
      utils.log("Iterating user equilibrium, epsilon = "+state._2)
      val shortestPathsLoading = loader.load(state._1.loadedNetwork,state._1.odPattern) // costs have already been updated
      val shortestPathsFlows = shortestPathsLoading.flows.toSeq.map{case (l,f) => (l.id,f)}.toMap
      val newLinkFlows: Map[Link,Double] = state._1.flows.toSeq.map{
        case (l,f) =>
          val newflow = (1 - descentStep)*f + descentStep*shortestPathsFlows.getOrElse(l.id,f)
          // assume there is no other weight process in the weight - must be included in the linkCostFunction
          (l.copy(weight=linkCostFunction(l,newflow)),newflow)
         }.toMap
        // update loaded network with new costs
      val newLinkFlowsIds = newLinkFlows.toSeq.map{case (l,f) => (l.id,f)}.toMap
      val delta = state._1.flows.toSeq.map{case (l,f) => math.abs(newLinkFlowsIds.getOrElse(l.id,f) - f) / f}.sum / newLinkFlows.size
      (state._1.updateFlows(newLinkFlows),delta)
    }

    Iterator.iterate((loader.load(network,odPattern).updateCosts(linkCostFunction),1.0.toDouble))(step).takeWhile(_._2>epsilon).toSeq.last._1

  }



}
