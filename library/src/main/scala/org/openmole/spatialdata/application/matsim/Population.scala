package org.openmole.spatialdata.application.matsim

object Population {

  /**
    * Convert spenser synth pop files to Matsim population
    *
    * Modes:
    *  - Uniform within MSOA, random job location within FUA
    *  - Buildings and population density, random job location
    *  - Buildings and population density, jobs extrapolated from commuting flows (spatial interaction model?)
    *
    * @param args arguments
    */
  def runPopulationConstruction(args: Array[String]): Unit = {
    println("Running population generation for MATSim model in UK")

    if(args.length<2) throw new IllegalArgumentException("Missing arguments; usage: --synthpop {$MODE in {--uniform,--detailedPopulation,--fourStage}} --FUAName=$NAME1,$NAME2,...,$NAMEN --FUAFile=$PATH --MSOAFile=$PATH")

    args(1) match {
      case "--uniform" => runUniformPopulation()
      case "--detailedPopulation" => runDetailedPopulation()
      case "--fourStage" => runFourStage()
      case _ => throw new IllegalArgumentException("Available modes: --uniform,--detailedPopulation,--fourStage")
    }

  }


  def runUniformPopulation(): Unit = {

  }


  def runDetailedPopulation(): Unit = {

  }

  def runFourStage(): Unit = {

  }

}
