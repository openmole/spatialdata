package org.openmole.spatialdata.application.urbanmorphology


import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.real.OSMGridSampling
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.osm.APIExtractor._

import scala.util.Random


/**
  * A runnable object to compute morphological measures on a set of OpenStreetMap samples
  */
object OSMGridRealMeasures extends App {

  if(args.length!=5) println("Usage : $NPOINTS $WINDOWSIZE(m) $WORLDWIDTH(cells) $SEED(0 = random Int) $RESDIR [$MODE = postgresql,osm,overpass]")

  val npoints = args(0).toInt // 10
  val windowSize = args(1).toInt // 500
  val worldWidth = args(2).toInt // 50
  val seed = args(3).toInt match {case s if s == 0.0 => {println("random seed");new Random().nextInt()};case s => s}
  val resdir = args(4)
  val mode: OSMAPIMode = if(args.length>=6) {
    args(5) match {
      case "overpass" => OSMOverpass
      case "osm" => OSMDirect
      case "mongo" => Mongo()
      case _ => Postgresql()
    }
  } else Postgresql()

  implicit val rng: Random = new Random(seed)

  val start = System.currentTimeMillis()

  val grids = OSMGridSampling.sampleGridsInLayer("data/cities_europe.shp",npoints,windowSize,worldWidth,mode=mode)
  val morphologies = grids.map{case g => GridMorphology(g._2)}
  val coordinates = grids.map{_._1}

  val data = morphologies.zip(coordinates).map{case (m,c)=>
    Array(c._1,c._2,m.height,m.width,m.area,m.avgBlockArea,m.avgComponentArea,m.avgDetour,m.avgDistance,m.components,m.density,m.fullClosingSteps,m.fullDilationSteps,m.fullErosionSteps,m.fullOpeningSteps,m.moran)}.toArray
  val header = Array("lon","lat","height","width","area","avgBlockArea","avgComponentArea","avgDetour","avgDistance","components","density","fullClosingSteps","fullDilationSteps","fullErosionSteps","fullOpeningSteps","moran")
  val resfile = resdir+"/morpho_npoints"+npoints+"_windowSize"+windowSize+"_worldWidth"+worldWidth+"_seed"+seed+".csv"

  CSV.writeCSV(data,resfile,";",header)

  println("Computation time : "+(System.currentTimeMillis()-start)/1000+" s")

}
