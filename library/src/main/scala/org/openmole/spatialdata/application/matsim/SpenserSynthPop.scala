package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata.vector.Point


case class SpenserSynthPop(
                          individuals: Seq[SpenserSynthPop.Individual],
                          households: Seq[SpenserSynthPop.Household]
                          )


object SpenserSynthPop {

  case class Individual(
                         msoaCode: String,
                         household: Household,
                         workLocation: Point
                       )

  case class Household(
                      homeLocation: Point
                      )

}
