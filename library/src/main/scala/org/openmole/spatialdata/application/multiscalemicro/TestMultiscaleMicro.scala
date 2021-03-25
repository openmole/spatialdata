package org.openmole.spatialdata.application.multiscalemicro

import scala.util.Random

object TestMultiscaleMicro extends App {

  MultiscaleMicro.visualize(
   MultiscaleMicro(
      worldSize = 10,
      patchSize = 1.0,
      nCenters = 3,
      mesoStepInterval = 5,
      steps = 1,
      seed = (new Random).nextLong(),
      transportationLinkSpeed = 5.0
    ).run()
  )
}
