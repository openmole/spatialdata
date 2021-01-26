package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata
import org.openmole.spatialdata.utils

object RunMatsim extends App {

  spatialdata.APPLICATION = true

  if (args.length>0) {
    args(0) match {
      case "--network" => Network.runNetworkProcessing(args)
      case "--synthpop" => Population.runPopulationConstruction(args)
      case s => utils.log("No action for "+s+"; usage: --network|--synthpop")
    }
  }

}
