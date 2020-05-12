package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.utils.math.{Linear, Matrix, Statistics}

import scala.util.Random

/**
  * Macroscopic state for a simple interaction model
  * @param time time step
  * @param populations populations of cities
  * @param distanceMatrix distance matrix - not cached as evolves at each step - must be from the beginning a generalized distance matrix ? (! update rule ?)
  * @param growthRates city specific growth rate (endogenous self-sustained growth)
  * @param interactionDecays city specific interaction decays (increase of accessibility for one city) rq : parameter can be used for coevolution ?
  * @param interactionWeights city specific interaction weights
  * @param interactionGammas city specific interaction gammas
  * @param congestedFlows congested flows
  */
case class InteractionMacroState(
                                  override val time: Int,
                                  override val populations: Matrix,
                                  override val distanceMatrix: Matrix,
                                  growthRates: Vector[Double],
                                  interactionDecays: Vector[Double],
                                  interactionWeights: Vector[Double],
                                  interactionGammas: Vector[Double],
                                  congestedFlows: Vector[Double]
                                ) extends MacroState

object InteractionMacroState {

  /**
    * initial hierarchical synthetic city system
    *
    * @param n number of cities
    * @param alpha initial hierarchy
    * @param pmax max initial population
    * @param worldSize world size
    * @param g0 gibrat growth
    * @param dg distance decay
    * @param wg interaction weight
    * @param gammag interaction gamma
    * @param rng rng
    * @return
    */
  def initialSyntheticState(n: Int,
                            alpha: Double,
                            pmax: Double,
                            worldSize: Double,
                            g0: Double,
                            dg: Double,
                            wg: Double,
                            gammag: Double
                           )(implicit rng: Random, mImpl: MatrixImplementation): InteractionMacroState = InteractionMacroState(
    time = 0,
    populations = Matrix(rng.shuffle(Statistics.rankSizeDistribution(n,alpha,pmax)).toArray,row=false),
    distanceMatrix = InteractionMacro.generalizedDistanceMatrix(Linear.randomDistanceMatrix(n,worldSize),Vector.fill(n)(dg)),
    Vector.fill(n)(g0),
    Vector.fill(n)(dg),
    Vector.fill(n)(wg),
    Vector.fill(n)(gammag),
    Vector.fill(n)(0.0)
  )

}
