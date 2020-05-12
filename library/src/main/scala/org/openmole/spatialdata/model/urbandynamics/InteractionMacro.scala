package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix}


object InteractionMacro {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  def deltaMacroStates(prev: MacroState,current: MacroState): Vector[(Double,Double,Double)] = {
    val prevIndics = prev.indicators
    val currentIndics = current.indicators
    // rq : assumed positive
    val deltaPops = prevIndics._1.zip(currentIndics._1).map{case (p,c) => c - p}
    val deltaPopsmax = deltaPops.max
    val deltaAccs = prevIndics._3.zip(currentIndics._3).map{case (p,c) => c - p}
    val deltaAccsmax = deltaAccs.max
    deltaPops.zip(deltaPops.map(_/deltaPopsmax)).zip(deltaAccs.map(_/deltaAccsmax)).map{case ((dp,dpr),dz) => (dp,dpr,dz)}
  }

  /**
    * macroscopic step
    * @param state macro state
    * @return
    */
  def macroStep(state: InteractionMacroState,
                additiveGrowthRates: Vector[Matrix => Matrix] = Vector.empty,
                deltat: Double = 1.0
               ): InteractionMacroState = {
    val growthRates: Matrix = additiveGrowthRates.map(_(state.populations)).reduce(Matrix.msum)
    val newpops = state.populations + (state.populations * deltat * growthRates)
    utils.log("macro Delta P = "+newpops.flatValues.zip(state.populations.flatValues).map{case (np,p)=>math.abs(np-p)}.sum)
    state.copy(time = state.time + 1,
      populations = newpops
    )
  }


  /**
    * Compute the evolution of macroscopic populations
    *
    *   ! no account of time step, by default 1 for synthetic application: generalize for possible calibration
    *
    * @param prevpop previous populations as a column vector
    * @param genDistanceMatrix generalized distance matrix
    * @param growthRates growth rate proper to each city
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
    val diagweights = DenseMatrix.diagonal(interactionWeights.toArray.map(_/ (n * meanpotgravity)))
    diagweights %*% potsgravity %*% DenseMatrix.ones(n, 1)
  }


  /**
    * Compute generalized dist mat from dist mat
    * as with d_{ij} the distance matrix, generalized distance is
    *  g_{ij} = exp( - d_{ij} / d_i)
    *
    * @param dmat distance matrix
    * @param decays interaction decays for each city
    * @return
    */
  def generalizedDistanceMatrix(dmat: Matrix, decays: Vector[Double]): Matrix =
    (DenseMatrix.diagonal(decays.toArray.map {1 / _}) %*% dmat).map(d => math.exp(d * (-1.0)))

  /**
    * Specific update for generalized distance given previous and new decays
    *  ( uses the fact that access is computed with an exponential)
    * @param dmat distance matrix
    * @param prevDecays previous decay
    * @param decays new decay
    * @return
    */
  def updateDistanceMatrix(dmat: Matrix,prevDecays: Vector[Double],decays: Vector[Double]): Matrix =
    dmat ^ (
      (DenseMatrix.diagonal(decays.toArray) %*% DenseMatrix.ones(decays.size,decays.size)).map(1/_) *  // 1 / d_i (t)
      (DenseMatrix.diagonal(prevDecays.toArray) %*% DenseMatrix.ones(prevDecays.size,prevDecays.size)) // d_i (t-1)
      )



}
