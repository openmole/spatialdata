package org.openmole.spatialdata.application.reactiondiffusion

import org.openmole.spatialdata.grid.synthetic.ReactionDiffusionGridGenerator
import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.measures.GridMorphology.{AverageDistance, Entropy, Moran, Slope}
import org.openmole.spatialdata.utils.math._

import scala.runtime.RichDouble
import scala.util.Random

case class ReactionDiffusionCalibration(
                                       initialConfiguration: Seq[Seq[Double]], // bad idea to have mutables as case class fields
                                       finalConfiguration: Seq[Seq[Double]],
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

  def runModel: (Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double) = {
    implicit val rng = new Random(seed)
    val (width,height) = (initialConfiguration(0).length,initialConfiguration.length)
    val deltaPop = popConstraint - initialConfiguration.flatten.filter(!_.isNaN).sum
    val growthrate = deltaPop / timeSteps
    if(verbose) println(s"Calibrating for initial area of size ${width}x${height} ; deltaPop = ${deltaPop} ; alpha = ${alpha} ; beta = ${beta} ; tsteps = ${timeSteps} ; growthRate = ${growthrate} ; seed = ${seed}")
    val generator = ReactionDiffusionGridGenerator((width,height),growthrate.toInt,popConstraint.toInt,alpha,beta,diffusionSteps.toInt,
      Some(initialConfiguration)
    )
    val generated = generator.generateGrid
    val finalPop = generated.flatten.filter(!_.isNaN).sum
    val morphology = GridMorphology(generated,Seq(Moran(),AverageDistance(),Entropy(),Slope()))
    // basic relative squared cost function
    if(verbose) println(s" Moran : ${morphology.moran} / ${moranObjective} ; avgDist : ${morphology.avgDistance} / ${avgDistObjective} ; Entropy : ${morphology.entropy} / ${entropyObjective} ; hierarchy : ${morphology.slope._1} / ${slopeObjective}")
    val mseindics = relSquare(moranObjective,morphology.moran) + relSquare(avgDistObjective,morphology.avgDistance) + relSquare(entropyObjective,morphology.entropy) + relSquare(slopeObjective,morphology.slope._1)
    if(verbose) println(s"Sq Rel Error = ${mseindics}")
    val poperror = relSquare(finalPop,popConstraint)
    if(verbose) println(s"Pop rel error = ${poperror}")
    // if(poperror > 0.1) Double.MaxValue else // do not do that, may bias selection through stoch outsiders - better include pop error in mse
    val msepop = generated.flatten.map{d => if (d.isNaN) 0.0 else d}.zip(finalConfiguration.flatten.map{d => if (d.isNaN) 0.0 else d}).map{case (p1,p2) => math.pow(p1 - p2,2)}.sum
    (msepop,mseindics + poperror,poperror,relSquare(moranObjective,morphology.moran),relSquare(avgDistObjective,morphology.avgDistance),
      relSquare(entropyObjective,morphology.entropy),relSquare(slopeObjective,morphology.slope._1),
      finalPop,morphology.moran,morphology.avgDistance,morphology.entropy,morphology.slope._1)
  }

}
