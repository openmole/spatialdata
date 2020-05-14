package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix}



/**
  * @param interactionDecays city specific interaction decays (increase of accessibility for one city) rq : parameter can be used for coevolution ?
  * @param interactionWeights city specific interaction weights
  * @param interactionGammas city specific interaction gammas
  * @param congestedFlows congested flows
  */
case class InteractionMacro(
   interactionDecays: Vector[Double],
   interactionWeights: Vector[Double],
   interactionGammas: Vector[Double],
   genDistanceMatrix: Matrix
) extends MacroGrowthRate {
  override def growthRate: Matrix => Matrix = {
    m =>
      InteractionMacro.interactionGrowthRates(m,genDistanceMatrix,interactionWeights,interactionGammas)
  }
}

object InteractionMacro {




  /**
    * Compute the evolution of macroscopic populations
    *
    *   ! no account of time step, by default 1 for synthetic application: generalize for possible calibration
    *
    * @param prevpop previous populations as a column vector
    * @param genDistanceMatrix generalized distance matrix
    * @param interactionWeights weights of interactions
    * @param interactionGammas gammas of interactions
    * @return next populations as a column vector
    */
  def interactionGrowthRates(prevpop: Matrix,
                      genDistanceMatrix: Matrix,
                      interactionWeights: Vector[Double],
                      interactionGammas: Vector[Double]): Matrix = {
    val n = prevpop.nrows
    val totalpop = prevpop.sum
    val diagpops = DenseMatrix.diagonal(prevpop.flatValues.zip(interactionGammas).map{ case (p,g) => math.pow(p / totalpop,g)})
    val potsgravity = diagpops %*% genDistanceMatrix %*% diagpops
    (0 until potsgravity.nrows).foreach(i => potsgravity.setM(i,i,0.0))
    val meanpotgravity = potsgravity.flatValues.sum / (n * n)
    utils.log("mean pot gravity : " + meanpotgravity)
    val diagweights = DenseMatrix.diagonal(interactionWeights.toArray.map(_/ (n * meanpotgravity)))
    diagweights %*% potsgravity %*% DenseMatrix.ones(n, 1)
  }



}
