package org.openmole.spatialdata.application.matsim

import java.io.{BufferedWriter, FileWriter}

import org.locationtech.jts.geom
import org.locationtech.jts.geom.GeometryFactory
import org.openmole.spatialdata.application.matsim.Matsim._
import org.openmole.spatialdata.application.matsim.SpenserSynthPop.{Household, Individual, Plan}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.gis.LayerSampling
import org.openmole.spatialdata.utils.io.{CSV, GIS}
import org.openmole.spatialdata.vector.{Attributes, Point, Polygons}

import scala.util.Random


object Population {

  val usage: String = "Missing arguments; usage:\n" +
    "--synthpop\n" +
    "--popMode={uniform|detailed}\n" +
    "--jobMode={random|sample|gravity}\n" +
    "--planMode={default}\n" +
    "--sample=$SAMPLE" +
    "--FUAName=$NAME1,$NAME2,...,$NAMEN\n" +
    "--FUAFile=$PATH\n" +
    "--LAFile=$PATH\n"+
    "--MSOAFile=$PATH\n" +
    "--SPENSERDir=$DIR\n" +
    "--output=$OUTPUT"

  val msoaID: String = "MSOA11CD"
  val ladID: String = "lad19cd"

  /**
    * Convert spenser synth pop files to Matsim population
    *
    * Modes:
    *  - Uniform within MSOA, random job location within FUA
    *  - Buildings and population density, random job location
    *  - Buildings and population density, jobs extrapolated from commuting flows (spatial interaction model?)
    *
    * Population distribution does not require network in Matsim spec (but same coordinate system): closest node is taken
    *
    * ! add optional seed argument?
    *
    * @param args arguments
    */
  def runPopulationConstruction(args: Array[String]): Unit = {

    implicit val rng: Random = new Random

    println("Running population generation for MATSim model in UK")

    // for test on Glasgow: Local Authorities S12000008 S12000011 S12000021 S12000029 S12000030 S12000038 S12000039 S12000045 S12000049 S12000050
    // runMain org.openmole.spatialdata.application.matsim.RunMatsim --synthpop --popMode=uniform --jobMode=random --planMode=default --sample=0.01 --FUAName=Glasgow --FUAFile=/Users/juste/ComplexSystems/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg --LAFile=/Users/juste/ComplexSystems/UrbanDynamics/Data/OrdnanceSurvey/LADistricts/Local_Authority_Districts__December_2019__Boundaries_UK_BUC-shp/LAD_WGS84.shp --MSOAFile=/Users/juste/ComplexSystems/UrbanDynamics/Data/QUANT/geography/EnglandWalesScotland_MSOAWGS84.shp --SPENSERDir=/Users/juste/ComplexSystems/UrbanDynamics/Data/SPENSER/2020/ --output=/Users/juste/ComplexSystems/UrbanDynamics/Models/Matsim/Population/test/Glasgow.xml

    if(args.length<2) throw new IllegalArgumentException(usage)



    val areas = loadAreas(parseArg(args, "FUAName").split(";").map(_.split(",").toSeq).toSeq, parseArg(args, "FUAFile"))
    val msoas = Polygons(GIS.readGeometry(parseArg(args, "MSOAFile"), Array(msoaID)))
    val lads = GIS.readGeometry(parseArg(args, "LAFile"), Array(ladID))
    //println(lads)
    val localAuthorities = Polygons(lads) // only used for synthpop but better consistence to have same level args

    areas.foreach { area =>
      val pop = loadSyntheticPopulation(area, localAuthorities, parseArg(args, "SPENSERDir"))

      val locator: SpenserSynthPop => SpenserSynthPop = parseArg(args, "popMode") match {
        case "uniform" => p: SpenserSynthPop => HomeLocation.uniformHomeLocationPopulation(p, msoas)
        case "detailed" => p: SpenserSynthPop => HomeLocation.detailedHomeLocationPopulation(p)
        case _ => throw new IllegalArgumentException("Available population modes: --popMode={uniform|detailed}")
      }

      val jobLocator: SpenserSynthPop => SpenserSynthPop = parseArg(args, "jobMode") match {
        case "random" => p: SpenserSynthPop => JobLocation.randomJobLocationPopulation(p, msoas)
        case "sample" => p: SpenserSynthPop => JobLocation.empiricalSamplingJobLocationPopulation(p)
        case "gravity" => p: SpenserSynthPop => JobLocation.gravityJobLocationPopulation(p)
        case _ => throw new IllegalArgumentException("Available job modes: --jobMode={random|sample|gravity}")
      }

      val planComposer: SpenserSynthPop => SpenserSynthPop = parseArg(args, "planMode") match {
        case "default" => p: SpenserSynthPop => PlanComposition.randomPlansPopulation(p)
        case _ => throw new IllegalArgumentException("Available plan modes: --planMode={default}")
      }


      val finalPopulation = (planComposer compose jobLocator compose locator) (pop).sample(parseArg(args, "sample").toDouble)

      println("Final population size = "+finalPopulation.individuals.size)

      // export the population
      //println(finalPopulation.individuals.map(_.homeLocation))
      exportMatsimXML(finalPopulation, parseArg(args, "output"))
    }
  }


  object HomeLocation {


    /**
      * Distribute population uniformally in MSOAs
      *
      * @param population spenser population
      * @param msoas      msoa polygons
      * @return
      */
    def uniformHomeLocationPopulation(population: SpenserSynthPop, msoas: Polygons)(implicit rng: Random): SpenserSynthPop = {
      val lochouseholds = population.households.map{household =>
        val (homemsoa,_) =  msoas.getPolygonByKeyValue(msoaID, household.msoaCode).get
        household.copy(homeLocation = LayerSampling.PolygonSampler(homemsoa).sample)
      }
      val hmap = lochouseholds.map(h => (h.hid,h)).toMap
      val locindividuals = population.individuals.map{individual =>
        individual.copy(homeLocation = hmap(individual.householdId).homeLocation)
      }
      SpenserSynthPop(locindividuals, lochouseholds)
    }


    /**
      * Detailed population
      *  - density grid
      *  - buildings (OSM)
      *
      * @param population population
      * @return
      */
    def detailedHomeLocationPopulation(population: SpenserSynthPop): SpenserSynthPop = {
      SpenserSynthPop(Seq.empty, Seq.empty)
    }
  }

  object JobLocation {

    /**
      * random job locations
      * @param population synthetic population
      * @return
      */
    def randomJobLocationPopulation(population: SpenserSynthPop, msoas: Polygons)(implicit rng: Random): SpenserSynthPop = {
      val polygonSampler = LayerSampling.PolygonSampler((new GeometryFactory).createMultiPolygon(msoas.polygons.toArray))
      val jobLocations: Seq[Point] = population.individuals.indices.map(_ => polygonSampler.sample)
      SpenserSynthPop(population.individuals.zip(jobLocations).map{case (ind,loc) => ind.copy(workLocation = loc)}, population.households)
    }

    /**
      * job location by sampling empirical census flows
      * @param population population
      * @return
      */
    def empiricalSamplingJobLocationPopulation(population: SpenserSynthPop): SpenserSynthPop = {
      SpenserSynthPop(Seq.empty, Seq.empty)
    }

    /**
      * First two stages of four stage model
      * @param population population
      * @return
      */
    def gravityJobLocationPopulation(population: SpenserSynthPop): SpenserSynthPop = {
      SpenserSynthPop(Seq.empty, Seq.empty)
    }
  }


  object PlanComposition {

    val workStartTimeUniformInterval: (Double,Double) = (7.0,10.0)
    val workEndTimeUniformInterval: (Double,Double) = (17.0,20.0)

    /**
      * Most basic plan home-work; assumes home and work loc are defined
      * @param population population
      * @return
      */
    def randomPlansPopulation(population: SpenserSynthPop)(implicit rng: Random): SpenserSynthPop =
      SpenserSynthPop(population.individuals.map(uniformTimesCommuterPlan), population.households)

    /**
      * ! only car commuting for now
      * @param individual individual
      * @param rng rng
      * @return
      */
    def uniformTimesCommuterPlan(individual: Individual)(implicit rng: Random): Individual = {
      val workStartTime = workStartTimeUniformInterval._1 + rng.nextDouble()*(workStartTimeUniformInterval._2 - workStartTimeUniformInterval._1)
      val workEndTime = workEndTimeUniformInterval._1 + rng.nextDouble()*(workEndTimeUniformInterval._2 - workEndTimeUniformInterval._1)
      individual.copy(plans = Seq(Plan.commutePlan(workStartTime, workEndTime, individual.homeLocation, individual.workLocation, "car")))
    }

  }

  /**
    * Load spenser files
    * @param area area
    * @param localAuthorities Polygons of local authorities
    * @return
    */
  def loadSyntheticPopulation(area: geom.Geometry, localAuthorities: Polygons, spenserDir: String): SpenserSynthPop = {
    val reqlads: Seq[(geom.Polygon,Attributes)] = localAuthorities.polygons.zip(localAuthorities.attributes).filter(_._1.intersects(area))
    //println(reqlads.map(_._2))
    val reqladcodes = reqlads.map(_._2.getOrElse("lad19cd",""))
    println("Req LAD codes: "+reqladcodes)
    val individuals: Seq[Individual] = reqladcodes.map{code =>
      utils.log("    loading individuals for LAD "+code)
      val indivcsv = CSV.readCSV(spenserDir+"/ass_"+code+"_MSOA11_2020.csv") // fixed file name - assume 2020?
      utils.log("    indivs: "+indivcsv.values.head.size)
      indivcsv.values.head.indices.map{ i =>
        Individual(indivcsv.keys.map(k => (k,indivcsv(k)(i))).toMap)
      }
    }.reduce(utils.concat[Individual])
    val households: Seq[Household] = reqladcodes.map{code =>
      println("    loading households for LAD "+code)
      val householdcsv = CSV.readCSV(spenserDir+"/ass_hh_"+code+"_OA11_2020.csv")
      householdcsv.values.head.indices.map{ i =>
        Household(householdcsv.keys.map(k => (k,householdcsv(k)(i))).toMap)
      }
    }.reduce(utils.concat[Household])
    SpenserSynthPop(individuals, households)
  }


  /**
    * Export to matsim xml population
    *
    * @param population population
    * @param file file
    */
  def exportMatsimXML(population: SpenserSynthPop, file: String): Unit = {
    val HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE population SYSTEM \"http://www.matsim.org/files/dtd/population_v1.dtd\">\n<population name=\"SPENSER/spatialdata generated population\">"
    val FOOTER = "</population>"
    val xml = new BufferedWriter(new FileWriter(file))
    xml.write(HEADER)

    population.individuals.foreach{person =>
      xml.write("<person id="+person.id+">\n")
      person.plans.foreach{plan =>
        xml.write("<plan>\n")
        xml.write(plan.actions.head.xml+"\n")
        plan.legs.zip(plan.actions.tail).foreach{case (leg,act) =>
          xml.write(leg.xml+"\n")
          xml.write(act.xml+"\n")
        }
        xml.write("</plan>\n")
      }
      xml.write("</person>\n\n")
    }

    xml.write(FOOTER)
    xml.close()
  }

}
