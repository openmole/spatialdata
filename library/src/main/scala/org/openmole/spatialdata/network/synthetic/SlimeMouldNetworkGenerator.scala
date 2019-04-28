package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.network._
import org.openmole.spatialdata.utils.math._

import scala.util.Random


case class SlimeMouldNetworkGenerator(
                                       gammaSlimeMould: Double,
                                       inputFlowSlimeMould: Double,
                                       deltatSlimeMould: Double = 0.05
                                     ) extends NetworkGenerator {
  override def generateNetwork(implicit rng: Random): Network = empty
}


object SlimeMouldNetworkGenerator {

  /**
    * An iteration of the slime mould : given pace matrix and current diameters, computes next diameters.
    * @param world
    * @param P
    * @param D
    * @return
    */
  def iterationSlimeMould(network: Network,P:Array[Array[Double]],D:Array[Array[Double]],slimeMould: SlimeMouldNetworkGenerator): Array[Array[Double]] = {
    // get od
    val (o,d)=chooseOD(network,Seq.empty)
    // io flows
    val ioflows = getIOFlows(o,d,D,slimeMould.inputFlowSlimeMould)
    // flows
    val flows = getFlowMatrix(D,P)
    // solve system
    val pressures = Linear.solveSystem(flows,ioflows)
    updateDiameters(D,P,pressures,slimeMould)
  }

  /**
    * update diameters
    * @param previousDiameters
    * @param pressures
    * @return
    */
  def updateDiameters(previousDiameters: Array[Array[Double]],
                      paceMatrix:Array[Array[Double]],
                      pressures: Option[Array[Double]],
                      slimeMould: SlimeMouldNetworkGenerator): Array[Array[Double]] = {
    pressures match {
      case None => previousDiameters
      case Some(press) =>
        // compute link flows
        previousDiameters.toSeq.zipWithIndex.map { case (r, i) => r.toSeq.zipWithIndex
          .map { case (v, j) => (v, (i, j)) }
        }.flatten
          .zip(paceMatrix.flatten.toSeq)
          .map { case ((d, (i, j)), p) =>
            val flow = math.pow(math.abs(d * p * (press(i) - press(j))), slimeMould.gammaSlimeMould)
            val deltad = flow / (1 + flow)
            deltad * slimeMould.deltatSlimeMould + (1 - slimeMould.deltatSlimeMould) * d
          }.toArray.sliding(previousDiameters.size, previousDiameters.size).toArray
    }
  }



  /**
    * convert an initial network to its adjacency matrix
    * assumes that node ids are in {0;|V|} ; network is not directed
    * @param network
    * @return
    */
  def networkToPaceMatrix(network: Network): Array[Array[Double]] = generalizedAdjacencyMatrix(1/_.length)(network)

  def initialDiameterMatrix(network: Network,initialDiameter: Double): Array[Array[Double]] = generalizedAdjacencyMatrix(_=>initialDiameter)(network)

  def generalizedAdjacencyMatrix(fun:Link=>Double)(network: Network): Array[Array[Double]] = {
    val n = network.nodes.size
    val M = Array.fill[Double](n,n)(0.0)
    for(link <- network.links){
      val (i,j) = (link.e1.id,link.e2.id)
      M(i)(j)=fun(link);M(j)(i)=fun(link)
    }
    M
  }

  /**
    * Get the matrix to be inverted, from diameter and pace matrix
    * @param D
    * @param P
    * @return
    */
  def getFlowMatrix(D: Array[Array[Double]],P: Array[Array[Double]]): Array[Array[Double]] = {
    val prod = D.flatten.toSeq.zip(P.flatten.toSeq).map{case(d,p)=> - d*p}.toArray.sliding(D.size,D.size).toArray
    for(i <- 0 to D.size - 1){prod(i)(i)= prod(i).sum}
    prod
  }

  /**
    * Get vector of io flows
    * @param origin index of origin node
    * @param destinations index of destination nodes
    * @param D
    * @return
    */
  def getIOFlows(origin: Int,destinations: Seq[Int],D: Array[Array[Double]],originFlow: Double): Array[Double] = {
    val res = Array.fill[Double](D.size)(0.0)
    res(origin)= originFlow
    for(d <- destinations){res(d) = - originFlow / destinations.size}
    res
  }


  /**
    * Choose o/d given the world and the network
    * @param world
    * @return indice of unique origin and indices of destination
    */
  def chooseOD(network: Network,odnodes: Seq[Node]): (Int,Seq[Int]) = {
    (0,Seq.empty)
    /*val omayor = Random.shuffle(world.mayors).take(1).toSeq(0)
    val o = omayor.position.number
    val dests = world.mayors.toSet.-(omayor).map{_.position.number}.toSeq
    (o,dests)
    */
  }




}
