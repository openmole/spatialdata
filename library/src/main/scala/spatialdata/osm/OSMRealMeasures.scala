package spatialdata.osm


import spatialdata.measures.Morphology
import spatialdata.sampling.OSMGridSampling
import spatialdata.utils.io.CSV

import scala.util.Random


object OSMRealMeasures extends App {

  if(args.length!=5) println("Usage : $NPOINTS $WINDOWSIZE(m) $WORLDWIDTH(cells) $SEED(0 = random Int) $RESDIR")

  val npoints = args(0).toInt // 10
  val windowSize = args(1).toInt // 500
  val worldWidth = args(2).toInt // 50
  val seed = args(3).toInt match {case s if s == 0.0 => {println("random seed");new Random().nextInt()};case s => s}
  val resdir = args(4)

  implicit val rng: Random = new Random(seed)

  val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",npoints,windowSize,worldWidth,mode="postgresql")
  val morphologies = grids.map{case g => Morphology(g._2)}
  val coordinates = grids.map{_._1}

  val data = morphologies.zip(coordinates).map{case (m,c)=>
    Array(c._1,c._2,m.height,m.width,m.area,m.avgBlockArea,m.avgComponentArea,m.avgDetour,m.avgDistance,m.components,m.density,m.fullClosingSteps,m.fullDilationSteps,m.fullErosionSteps,m.fullOpeningSteps,m.moran)}.toArray
  val header = Array("lon","lat","height","width","area","avgBlockArea","avgComponentArea","avgDetour","avgDistance","components","density","fullClosingSteps","fullDilationSteps","fullErosionSteps","fullOpeningSteps","moran")
  val resfile = resdir+"/morpho_npoints"+npoints+"_windowSize"+windowSize+"_worldWidth"+worldWidth+"_seed"+seed+".csv"

  CSV.writeCSV(data,resfile,";",header)

}

