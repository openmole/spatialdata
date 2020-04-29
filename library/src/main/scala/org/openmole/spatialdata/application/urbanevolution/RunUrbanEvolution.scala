package org.openmole.spatialdata.application.urbanevolution

import org.openmole.spatialdata.model.urbandynamics.Innovation
import org.openmole.spatialdata.utils.math.Statistics

import scala.util.Random

object RunUrbanEvolution extends App {

  implicit val rng: Random = new Random

  val model = Innovation(
    syntheticCities = 30,
    syntheticHierarchy = 1.0,
    syntheticMaxPop =  100000.0,
    finalTime = 50,
    seed = rng.nextInt(),
    growthRate = 0.0,
    innovationWeight = 0.005,
    gravityDecay = 0.5,
    innovationDecay = 0.3,
    mutationRate = 0.4,
    newInnovationHierarchy = 0.5,
    earlyAdoptersRate = 0.2,
    utilityStd = 1.0,
    utilityDistribution = "normal"//"log-normal"
  )

  val result = Innovation.run(model)

  println("Average diversity = "+result.averageDiversity)
  println("Average utility = "+result.averageUtility)
  println("Average innovation = "+result.averageInnovation)
  val finalHierarchy = Statistics.slope(result.macroResult.simulatedPopulation.getCol(50).flatValues)._1
  println("Final pop hierarchy = "+finalHierarchy)
  println("Delta hierarchy pop = "+(finalHierarchy - Statistics.slope(result.macroResult.simulatedPopulation.getCol(0).flatValues)._1))

}
