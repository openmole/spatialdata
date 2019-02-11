package spatialdata.measures


import spatialdata.sampling.OSMGridSampling
import spatialdata.utils.io.CSV

import scala.util.Random


object OSMRealMeasures extends App {

  val npoints = args(0).toInt // 10
  val windowSize = args(1).toInt // 500
  val worldWidth = args(2).toInt // 50
  val seed = args(3).toInt
  val resdir = args(4)

  implicit val rng: Random = new Random(seed)

  val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",npoints,windowSize,worldWidth)
  val morphologies = grids.map{case g => Morphology(g._2)}
  val coordinates = grids.map{_._1}

  CSV.writeCSV(morphologies.zip(coordinates).map{case (m,c)=>Array(c._1,c._2,m.avgBlockArea,m.avgDetour,m.avgDistance,m.components,m.density,m.fullClosingSteps,m.fullDilationSteps,m.fullErosionSteps,m.fullOpeningSteps)}.toArray,resdir+"/morpho_npoints"+npoints+"_windowSize"+windowSize+"_worldWidth"+worldWidth+"_seed"+seed+".csv",";")

}

