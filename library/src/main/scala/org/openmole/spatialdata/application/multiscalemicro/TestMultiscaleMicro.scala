package org.openmole.spatialdata.application.multiscalemicro

import scala.util.Random

// runMain org.openmole.spatialdata.application.multiscalemicro.TestMultiscaleMicro
object TestMultiscaleMicro extends App {

  MultiscaleMicro.visualize(
   MultiscaleMicro(
      worldSize = 10,
      nCenters = 3,
      mesoStepInterval = 5,
      steps = 1,
      seed = (new Random).nextLong(),
      transportationLinkSpeed = 5.0,
      developersNumber = 5,
      developerSetupMode = "uniform"
    ).run()
  )
}
