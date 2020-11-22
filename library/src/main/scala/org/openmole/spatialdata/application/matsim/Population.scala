package org.openmole.spatialdata.application.matsim

import org.locationtech.jts.geom
import Matsim._
import org.openmole.spatialdata.application.matsim.SpenserSynthPop.{Household, Individual}
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

    val population = args(1) match {
      case "--uniform" => uniformPopulation(pop, msoas)
      case "--detailedPopulation" => detailedPopulation()
      case "--fourStage" => fourStagePopulation()
      case _ => throw new IllegalArgumentException("Available modes: --uniform,--detailedPopulation,--fourStage")
    }

    // export the population
    println(population.individuals.map(_.homeLocation))

  }


  /**
    * Distribute population uniformally in MSOAs
    * @param population spenser population
    * @param msoas msoa polygons
    * @return
    */
  def uniformPopulation(population: SpenserSynthPop, msoas: Polygons): SpenserSynthPop = {
    //population.households.map()
    SpenserSynthPop(Seq.empty, Seq.empty)
  }


  def detailedPopulation(): SpenserSynthPop = {
    SpenserSynthPop(Seq.empty, Seq.empty)
  }

  def fourStagePopulation(): SpenserSynthPop = {
    SpenserSynthPop(Seq.empty, Seq.empty)
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
    val individuals: Seq[Individual] = reqladcodes.map{code =>
      val indivcsv = CSV.readCSV(spenserDir+"/ass_"+code+"_MSOA11_2020.csv") // fixed file name - assume 2020?
      indivcsv.values.head.indices.map{ i =>
        Individual(indivcsv.keys.map(k => (k,indivcsv(k)(i))).toMap)
      }
    }.reduce{case (s1,s2)=> s1++s2}
    val households: Seq[Household] = reqladcodes.map{code =>
      val householdcsv = CSV.readCSV(spenserDir+"/ass_hh_"+code+"_OA11_2020.csv")
      householdcsv.values.head.indices.map{ i =>
        Household(householdcsv.keys.map(k => (k,householdcsv(k)(i))).toMap)
      }
    }.reduce{case (s1,s2)=> s1++s2}
    SpenserSynthPop(individuals, households)
  }

}
