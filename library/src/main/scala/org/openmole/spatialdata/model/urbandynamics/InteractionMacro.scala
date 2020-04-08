package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix}


object InteractionMacro {


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
  def macroStep(state: InteractionMacroState): InteractionMacroState = {
    val newpops = interactionStep(state.populations,state.distanceMatrix,state.growthRates,state.interactionWeights,state.interactionGammas)
    utils.log("macro Delta P = "+newpops.zip(state.populations).map{case (np,p)=>math.abs(np-p)}.sum)
    state.copy(time = state.time + 1,
      populations = newpops
    )
  }


  /**
    * Compute the evolution of macroscopic populations
    *
    * @param prevpop previous populations
    * @param genDistanceMatrix
    * @param growthRates growth rate proper to each city
    * @param interactionWeights
    * @param interactionGammas
    * @return
    */
  def interactionStep(prevpop: Vector[Double],genDistanceMatrix: Matrix,growthRates: Vector[Double],interactionWeights: Vector[Double],interactionGammas: Vector[Double]): Vector[Double] = {
    val delta_t = 1 // synthetic model // TODO generalize for possible calibration
    val n = prevpop.size
    val totalpop = prevpop.toArray.sum
    val diagpops = DenseMatrix.diagonal(prevpop.toArray.zip(interactionGammas).map{ case (p,g) => math.pow(p / totalpop,g)})
    //utils.log("diagpops : "+diagpops.getRowDimension+"x"+diagpops.getColumnDimension+" ; dmat : "+dmat.getRowDimension+"x"+dmat.getColumnDimension)
    val potsgravity = diagpops %*% genDistanceMatrix %*% diagpops
    (0 until potsgravity.nrows).foreach(i => potsgravity.setM(i,i,0.0))
    val meanpotgravity = potsgravity.flatValues.sum / (prevpop.size * prevpop.size)
    val diagweights = DenseMatrix.diagonal(interactionWeights.toArray.map(_/ (n * meanpotgravity)))
    val prevpopmat = Matrix(prevpop.toArray.map(Array(_)))(Matrix.defaultImplementation)
    (prevpopmat +
       (
         (prevpopmat * delta_t) *
           (
             (diagweights %*% potsgravity %*% DenseMatrix.ones(n, 1)) +
                Matrix(growthRates.toArray.map(Array(_)))(Matrix.defaultImplementation)
           )
        )
     ).values.transpose.head.toVector
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
    * @param dmat
    * @param prevDecays
    * @param decays
    * @return
    */
  def updateDistanceMatrix(dmat: Matrix,prevDecays: Vector[Double],decays: Vector[Double]): Matrix =
    dmat ^ (
      (DenseMatrix.diagonal(decays.toArray) %*% DenseMatrix.ones(decays.size,decays.size)).map(1/_) *  // 1 / d_i (t)
      (DenseMatrix.diagonal(prevDecays.toArray) %*% DenseMatrix.ones(prevDecays.size,prevDecays.size)) // d_i (t-1)
      )



}
