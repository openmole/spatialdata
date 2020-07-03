package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.utils.math.{DenseMatrix, Linear, Matrix, Statistics}

import scala.util.Random

/**
  * A macroscopic urban dynamic model
  */
trait MacroModel {
  def run: MacroResult
}



object MacroModel {
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
    * Macroscopic step, following a superposition of processes driving growth rates
    *  Population is updated as (P(t+1) - P(t)) / P(t) = delta_t* total growth rate
    *
    * @param state macro state before the step
    * @param additiveGrowthRates components to sum for the total growth rate
    * @param deltat time difference
    * @return
    */
  def macroStep(state: MacroState,
                additiveGrowthRates: Vector[MacroGrowthRate] = Vector.empty,
                deltat: Double = 1.0
               ): MacroState = {
    val growthRates: Matrix = additiveGrowthRates.map(_.growthRate(state.populations)).reduce(Matrix.msum)
    val newpops = state.populations + (state.populations * deltat * growthRates)
    utils.log("macro Delta P = "+newpops.flatValues.zip(state.populations.flatValues).map{case (np,p)=>math.abs(np-p)}.sum)
    state.copy(time= state.time + 1, populations = newpops)
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



  /**
    * initial hierarchical synthetic city system
    *
    * @param n number of cities
    * @param alpha initial hierarchy
    * @param pmax max initial population
    * @param worldSize world size
    * @param dg distance decay
    * @param rng rng
    * @return
    */
  def initialSyntheticState(n: Int,
                            alpha: Double,
                            pmax: Double,
                            worldSize: Double,
                            dg: Double
                           )(implicit rng: Random, mImpl: MatrixImplementation): MacroState =
    MacroState(
     time = 0,
     populations = Matrix(rng.shuffle(Statistics.rankSizeDistribution(n, alpha, pmax)).toArray, row = false),
     distanceMatrix = generalizedDistanceMatrix(Linear.randomDistanceMatrix(n, worldSize), Vector.fill(n)(dg))
    )


}
