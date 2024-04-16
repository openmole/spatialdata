package org.openmole.spatialdata.application.swarmchemistry

import scala.util.Random

object RunSwarmChemistry extends App {

  implicit val rng: Random = Random
  implicit val model: SwarmChemistry = SwarmChemistry()

  SwarmChemistry.runSwarmChemistry("random:99", "uniform")

  println(SwarmChemistry.indicators)

}
