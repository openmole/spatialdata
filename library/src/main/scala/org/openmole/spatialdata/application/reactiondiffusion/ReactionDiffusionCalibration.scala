package org.openmole.spatialdata.application.reactiondiffusion

import org.openmole.spatialdata.grid.synthetic.ReactionDiffusionGridGenerator
import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.measures.GridMorphology.{AverageDistance, Entropy, Moran, Slope}

import scala.runtime.RichDouble
import scala.util.Random

case class ReactionDiffusionCalibration(
                                       initialConfiguration: Array[Array[Double]],
                                       alpha: Double,
                                       beta: Double,
                                       diffusionSteps: Double,
                                       timeSteps: Double,
                                       popConstraint: Double,
                                       moranObjective: Double,
                                       avgDistObjective: Double,
                                       entropyObjective: Double,
                                       slopeObjective: Double,
                                       seed: Long,
                                       verbose: Boolean = true
                                       ) {

  def relSquare(x: Double,y: Double): Double = math.pow(2*(x - y)/(x+y) ,2)

  def runModel: Double = {
    implicit val rng = new Random(seed)
    val (width,height) = (initialConfiguration(0).length,initialConfiguration.length)
    val deltaPop = popConstraint - initialConfiguration.flatten.filter(!_.isNaN).sum
    val growthrate = deltaPop / timeSteps
    if(verbose) println(s"Calibrating for initial area of size ${width}x${height} ; deltaPop = ${deltaPop} ; alpha = ${alpha} ; beta = ${beta} ; tsteps = ${timeSteps} ; growthRate = ${growthrate} ; seed = ${seed}")
    val generator = ReactionDiffusionGridGenerator((width,height),growthrate.toInt,popConstraint.toInt,alpha,beta,diffusionSteps.toInt,
      Some(initialConfiguration)
    )
    val morphology = GridMorphology(generator.generateGrid,Seq(Moran(),AverageDistance(),Entropy(),Slope()))
    // basic relative squared cost function
    val mse = relSquare(moranObjective,morphology.moran) + relSquare(avgDistObjective,morphology.avgDistance) + relSquare(entropyObjective,morphology.entropy) + relSquare(slopeObjective,morphology.slope._1)
    if(verbose) println(s"Sq Rel Error = ${mse}")
    mse
  }

}
