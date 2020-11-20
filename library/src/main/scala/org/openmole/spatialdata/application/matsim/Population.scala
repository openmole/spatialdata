package org.openmole.spatialdata.application.matsim

import org.locationtech.jts.geom
import Matsim._
import org.openmole.spatialdata.utils.io.{CSV, GIS}
import org.openmole.spatialdata.vector.{Attributes, Polygons}

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

    // for test on Glasgow: Local Authorities S12000008 S12000011 S12000021 S12000029 S12000030 S12000038 S12000039 S12000045 S12000049 S12000050

    if(args.length<2) throw new IllegalArgumentException("" +
      "Missing arguments; usage: " +
      "--synthpop {$MODE in {--uniform,--detailedPopulation,--fourStage}} " +
      "--FUAName=$NAME1,$NAME2,...,$NAMEN " +
      "--FUAFile=$PATH " +
      "--LAFile=$PATH "+
      "--MSOAFile=$PATH " +
      "--SPENSERDir")

    val area = loadArea(parseArg(args, "FUAName").split(","), parseArg(args, "FUAFile"))
    val msoas = Polygons(GIS.readGeometry(parseArg(args, "MSOAFile"), Array("MSOA11CD")))
    val localAuthorities = Polygons(GIS.readGeometry(parseArg(args, "LAFile"), Array("lad19cd"))) // only used for synthpop but better consistence to have same level args
    val pop = loadSyntheticPopulation(area, localAuthorities, parseArg(args, "SPENSERDir"))

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

  /**
    * Load spenser files
    * @param area area
    * @param localAuthorities Polygons of local authorities
    * @return
    */
  def loadSyntheticPopulation(area: geom.Geometry, localAuthorities: Polygons, spenserDir: String): SpenserSynthPop = {
    val reqlads: Seq[(geom.Polygon,Attributes)] = localAuthorities.polygons.zip(localAuthorities.attributes).filter(_._1.intersects(area))
    val reqladcodes = reqlads.map(_._2.getOrElse("lad19cd",""))
    reqladcodes.map{code => CSV.readCSV("")}
    SpenserSynthPop(Seq.empty, Seq.empty)
  }

}
