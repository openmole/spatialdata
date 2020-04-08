package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.{Linear, Matrix, Statistics}

import scala.util.Random

case class InteractionMacroState(
                                  override val time: Int,

                                  /**
                                    * populations of cities
                                    */
                                  override val populations: Vector[Double],

                                  /**
                                    * distance matrix - not cached as evolves at each step
                                    *  - must be from the beginning a generalized distance matrix ? (! update rule ?)
                                    */
                                  override val distanceMatrix: Matrix,

                                  /**
                                    * city specific growth rate (endogenous self-sustained growth)
                                    */
                                  growthRates: Vector[Double],

                                  /**
                                    * city specific interaction decays (increase of accessibility for one city)
                                    *  rq : parameter can be used for coevolution ?
                                    */
                                  interactionDecays: Vector[Double],

                                  interactionWeights: Vector[Double],

                                  /**
                                    * city specific interaction gamma
                                    */
                                  interactionGammas: Vector[Double],


                                  congestedFlows: Vector[Double]
                                ) extends MacroState

object InteractionMacroState {

  /**
    * initial hierarchical synthetic city system
    *
    * @param n
    * @param alpha
    * @param pmax
    * @param worldSize
    * @param g0
    * @param dg
    * @param wg
    * @param gammag
    * @param rng
    * @return
    */
  def initialSyntheticState(n: Int,alpha: Double,pmax: Double,worldSize: Double,g0: Double,dg: Double,wg: Double,gammag: Double)(implicit rng: Random): InteractionMacroState = InteractionMacroState(
    0,
    rng.shuffle(Statistics.rankSizeDistribution(n,alpha,pmax)),
    InteractionMacro.generalizedDistanceMatrix(Linear.randomDistanceMatrix(n,worldSize),Vector.fill(n)(dg)),
    Vector.fill(n)(g0),
    Vector.fill(n)(dg),
    Vector.fill(n)(wg),
    Vector.fill(n)(gammag),
    Vector.fill(n)(0.0)
  )

}
