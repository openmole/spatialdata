package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.grid.synthetic.{ExpMixtureGridGenerator, ReactionDiffusionGridGenerator}
import org.openmole.spatialdata.grid.Implicits._

import scala.util.Random

case class ReactionDiffusionMesoState(
                                       override val time: Int,
                                       override val populationGrid: Vector[Vector[Double]],
                                       alpha: Double,
                                       beta: Double,
                                       ndiff: Int,
                                       growthRate: Double,
                                       mesoTimeSteps: Int,
                                       missingPopulation: Double
                                     ) extends MesoState {

}


object ReactionDiffusionMesoState {

  /**
    * monocentric initial metropolitan config
    * @param gridSize
    * @return
    */
  def initialSyntheticState(gridSize: Int,centerDensity: Double,kernelRadius: Double,
                            alpha: Double,beta: Double,ndiff: Int,growthRate: Double,
                            mesoTimeSteps: Int)(implicit rng: Random): ReactionDiffusionMesoState = ReactionDiffusionMesoState(
    0,
    ExpMixtureGridGenerator(gridSize,1,centerDensity,kernelRadius,false,Seq((gridSize/2,gridSize/2))).generateGrid.map{_.toVector}.toVector,
    alpha,beta,ndiff,growthRate,mesoTimeSteps,0.0
  )



  /**
    * iterate a meso step for a set of meso states
    * @param states
    * @param rng
    * @return
    */
  def mesoStep(states: Vector[ReactionDiffusionMesoState])(implicit rng: Random): Vector[ReactionDiffusionMesoState] = {
    states.map{
      s => {
        val toadd = s.growthRate*s.mesoTimeSteps
        val prevpop = s.populationGrid.flatten.sum
        var state = s
        (0 until s.mesoTimeSteps).foreach{_ =>
          state = state.copy(time=state.time+1,populationGrid=ReactionDiffusionGridGenerator.reactionDiffusionGrid(0,s.growthRate,s.alpha,s.beta,s.ndiff,(prevpop+toadd).toInt,Some(state.populationGrid)).map(_.toVector).toVector)
        }
        val newpop = state.populationGrid.flatten.sum
        state.copy(missingPopulation = toadd-(newpop-prevpop))
      }
    }
  }


}
