package org.openmole.spatialdata.application.matsim

import java.io.{BufferedWriter, File, FileWriter}

import org.locationtech.jts.geom
import org.locationtech.jts.geom.GeometryFactory
import org.openmole.spatialdata.application.matsim.Matsim._
import org.openmole.spatialdata.application.matsim.SpenserSynthPop.{Household, Individual, Plan}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.gis.LayerSampling
import org.openmole.spatialdata.utils.io.{CSV, GIS}
import org.openmole.spatialdata.utils.math.Stochastic
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
    "--OAFile=$PATH\n" +
    "--SPENSERDirs=$DIR\n" +
    "--output=$OUTPUT"

  val oaID: String = "OA11CD"
  val ladID: String = "lad19cd"

  def spenserPopFile(code: String): String = "ass_"+code+"_MSOA11_2020.csv"
  def spenserHouseholdFile(code: String): String = "ass_hh_"+code+"_OA11_2020.csv"

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

    utils.log("Running population generation for MATSim model in UK")

    // for test on Glasgow: Local Authorities S12000008 S12000011 S12000021 S12000029 S12000030 S12000038 S12000039 S12000045 S12000049 S12000050
    // runMain org.openmole.spatialdata.application.matsim.RunMatsim --synthpop --popMode=uniform --jobMode=random --planMode=default --sample=0.01 --FUAName=Glasgow --FUAFile=/Users/juste/ComplexSystems/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg --LAFile=/Users/juste/ComplexSystems/UrbanDynamics/Data/OrdnanceSurvey/LADistricts/Local_Authority_Districts__December_2019__Boundaries_UK_BUC-shp/LAD_WGS84.shp --OAFile=/Users/juste/ComplexSystems/UrbanDynamics/Data/QUANT/geography/EnglandWalesScotland_MSOAWGS84.shp --SPENSERDirs=/Users/juste/ComplexSystems/UrbanDynamics/Data/SPENSER/2020/ --output=/Users/juste/ComplexSystems/UrbanDynamics/Models/Matsim/Population/test/Glasgow.xml
    //
    // Test on Exeter: LADs E07000040 E07000041 E07000042 E07000045 E07000047
    //runMain org.openmole.spatialdata.application.matsim.RunMatsim --synthpop --popMode=uniform --jobMode=random --planMode=default --sample=0.01 --FUAName=Exeter --FUAFile=/Users/juste/ComplexSystems/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg --LAFile=/Users/juste/ComplexSystems/UrbanDynamics/Data/OrdnanceSurvey/LADistricts/Local_Authority_Districts__December_2019__Boundaries_UK_BUC-shp/LAD_WGS84.shp --OAFile=/Users/juste/ComplexSystems/UrbanDynamics/Data/QUANT/geography/EnglandWalesScotland_MSOAWGS84.shp --SPENSERDirs=/Users/juste/ComplexSystems/UrbanDynamics/Data/SPENSER/2020/England,/Users/juste/ComplexSystems/UrbanDynamics/Data/SPENSER/2020/Scotland,/Users/juste/ComplexSystems/UrbanDynamics/Data/SPENSER/2020/Wales --output=/Users/juste/ComplexSystems/UrbanDynamics/Models/Matsim/Population/test/Exeter.xml
    // Test on Hereford: LADs
    //runMain org.openmole.spatialdata.application.matsim.RunMatsim --synthpop --popMode=uniform --jobMode=random --planMode=default --sample=0.01 --FUAName=Hereford --FUAFile=/Users/juste/ComplexSystems/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg --LAFile=/Users/juste/ComplexSystems/UrbanDynamics/Data/OrdnanceSurvey/LADistricts/Local_Authority_Districts__December_2019__Boundaries_UK_BUC-shp/LAD_WGS84.shp --OAFile=/Users/juste/ComplexSystems/Data/OrdnanceSurvey/Output_Areas__December_2011__Boundaries_EW_BGC-shp/OA2011_WGS84.shp --SPENSERDirs=/Users/juste/ComplexSystems/UrbanDynamics/Data/SPENSER/2020/England,/Users/juste/ComplexSystems/UrbanDynamics/Data/SPENSER/2020/Scotland,/Users/juste/ComplexSystems/UrbanDynamics/Data/SPENSER/2020/Wales --output=/Users/juste/ComplexSystems/UrbanDynamics/Models/Matsim/Population/test/Hereford.xml


    if(args.length<2) throw new IllegalArgumentException(usage)


    val fuanames= parseArg(args, "FUAName").replace("\"","").split(";").toSeq
    val areas = loadAreas(fuanames.map(_.split(",").toSeq), parseArg(args, "FUAFile"))
    val oas = Polygons(GIS.readGeometry(parseArg(args, "OAFile"), Array(oaID)))
    val lads = GIS.readGeometry(parseArg(args, "LAFile"), Array(ladID))
    val localAuthorities = Polygons(lads) // only used for synthpop but better consistence to have same level args

    val sampling = parseArg(args, "sample").toDouble

    areas.zip(fuanames).foreach { case (area,name) =>
      utils.log("Constructing synthetic population for FUA "+name)
      val pop = loadSyntheticPopulation(area, localAuthorities, parseArg(args, "SPENSERDirs").split(","), sampling = sampling)

      val locator: SpenserSynthPop => SpenserSynthPop = parseArg(args, "popMode") match {
        case "uniform" => p: SpenserSynthPop => HomeLocation.uniformHomeLocationPopulation(p, oas)
        case "detailed" => p: SpenserSynthPop => HomeLocation.detailedHomeLocationPopulation(p)
        case _ => throw new IllegalArgumentException("Available population modes: --popMode={uniform|detailed}")
      }

      val jobLocator: SpenserSynthPop => SpenserSynthPop = parseArg(args, "jobMode") match {
        case "random" => p: SpenserSynthPop => JobLocation.randomJobLocationPopulation(p, area)
        case "sample" => p: SpenserSynthPop => JobLocation.empiricalSamplingJobLocationPopulation(p)
        case "gravity" => p: SpenserSynthPop => JobLocation.gravityJobLocationPopulation(p)
        case _ => throw new IllegalArgumentException("Available job modes: --jobMode={random|sample|gravity}")
      }

      val planComposer: SpenserSynthPop => SpenserSynthPop = parseArg(args, "planMode") match {
        case "default" => p: SpenserSynthPop => PlanComposition.randomPlansPopulation(p)
        case _ => throw new IllegalArgumentException("Available plan modes: --planMode={default}")
      }

      // note: sampling in the end takes much more memory as all population must be constructed (including Plan objects, etc.)
      val finalPopulation = (planComposer compose jobLocator compose locator) (pop)//.sample(parseArg(args, "sample").toDouble)

      utils.log("Final population size = "+finalPopulation.individuals.size)

      // export the population
      exportMatsimXML(finalPopulation, parseArg(args, "output")+name.replace(",","_")+".xml")
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
      val lochouseholds = population.households.groupBy(_.oaCode).map{case (oaCode,hs) =>
        val (homeoa,_) =  msoas.getPolygonByKeyValue(oaID, oaCode).get
        //val (c,e) = (homeoa.getCentroid, homeoa.getEnvelopeInternal)
        val e = homeoa.getEnvelopeInternal
        //val (x, y, xmin, xmax, ymin, ymax) = (c.getX, c.getY, e.getMinX, e.getMaxX, e.getMinY, e.getMaxY)
        val (xmin, xmax, ymin, ymax) = (e.getMinX, e.getMaxX, e.getMinY, e.getMaxY)
        hs.map{household =>
          //val homeloc = LayerSampling.sampleEnvelope(homeoa, 1).head
          // val homeloc =  LayerSampling.PolygonSampler(homeoa, 0.001).sample // issue with the polygon sampler
          household.copy(homeLocation = (xmin + rng.nextDouble*(xmax-xmin), ymin+rng.nextDouble*(ymax-ymin)))
        }
      }.reduce(utils.concat[Household])
      utils.log("Located households = "+lochouseholds.size)
      val hmap = lochouseholds.map(h => (h.hid,h)).toMap
      val indwithouthidcount = population.individuals.count(i => hmap.contains(i.householdId))
      utils.log("indiv with households % = "+(100*indwithouthidcount.toDouble/population.individuals.size.toDouble))
      val locindividuals = population.individuals.flatMap{individual =>
        if (hmap.contains(individual.householdId)) Some(individual.copy(homeLocation = hmap(individual.householdId).homeLocation)) else None
      }
      utils.log("Located individuals = "+locindividuals.size)
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
    def randomJobLocationPopulation(population: SpenserSynthPop, area: geom.Geometry)(implicit rng: Random): SpenserSynthPop = {
      val polygonSampler = LayerSampling.PolygonSampler((new GeometryFactory).createMultiPolygon(Array(area.asInstanceOf[geom.Polygon])))
      utils.log("Sampling jobs randomly into polygon of area "+area.getArea)
      val jobLocations: Seq[Point] = population.individuals.indices.map(_ => polygonSampler.sample)
      utils.log("Job locations = "+jobLocations.size)
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
    def randomPlansPopulation(population: SpenserSynthPop)(implicit rng: Random): SpenserSynthPop = {
      utils.log("Random plans for individuals")
      SpenserSynthPop(population.individuals.map(uniformTimesCommuterPlan), population.households)
    }

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
  def loadSyntheticPopulation(area: geom.Geometry,
                              localAuthorities: Polygons,
                              spenserDirs: Array[String],
                              indivFileName: (String,String)=>String = {case (dir,code) =>dir+"/"+spenserPopFile(code) },
                              householdFileName: (String,String)=>String = {case (dir,code) =>dir+"/"+spenserHouseholdFile(code) },
                              sampling: Double = 1.0
                             )(implicit rng: Random): SpenserSynthPop = {
    val reqlads: Seq[(geom.Polygon,Attributes)] = localAuthorities.polygons.zip(localAuthorities.attributes).filter(_._1.intersects(area))
    val reqladcodes = reqlads.map(_._2.getOrElse(ladID,"").toString)
    utils.log("Req LAD codes: "+reqladcodes)
    val individuals: Seq[Individual] = reqladcodes.map{code =>
      utils.log("    loading individuals for LAD "+code)
      val potfiles = spenserDirs.map(d => indivFileName(d,code)).filter(new File(_).exists())
      //if (potfiles.isEmpty) throw new RuntimeException("Population file could not be found for LAD: "+code)
      if (potfiles.isEmpty) Seq.empty
      else {
        val indivcsv = CSV.readCSVraw(potfiles.head, header = false) // take the first available file
        utils.log("    indivs: " + indivcsv.length)
        indivcsv.map(r => Individual(r, Individual.csvIndices)).toSeq
      }
    }.reduce(utils.concat[Individual])

    val sampledindivs = if (sampling==1.0) individuals else Stochastic.sampleWithoutReplacement(individuals, (sampling*individuals.size).toInt)
    val hids = sampledindivs.map(_.householdId).toSet

    val households: Seq[Household] = reqladcodes.map{code =>
      utils.log("    loading households for LAD "+code)
      val potfiles = spenserDirs.map(d => householdFileName(d,code)).filter(new File(_).exists())
      //if (potfiles.isEmpty) throw new RuntimeException("Household file could not be found for LAD: "+code)
      if (potfiles.isEmpty) Seq.empty
      else {
        val householdcsv = CSV.readCSVraw(potfiles.head, header = false)
        utils.log("    households: " + householdcsv.length)
        householdcsv.flatMap(r => if (hids.contains(r(Household.csvIndices.head).toInt)) Some(Household(r, Household.csvIndices)) else None).toSeq
      }
    }.reduce(utils.concat[Household])
    utils.log("Sampled pop size = "+sampledindivs.size+" ; "+households.size)
    SpenserSynthPop(sampledindivs, households)
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
