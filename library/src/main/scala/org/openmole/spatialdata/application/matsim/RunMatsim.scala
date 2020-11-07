package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata.network.real.{GISFileNetworkGenerator, MatsimNetworkGenerator}
import org.openmole.spatialdata.utils.io.{GeoPackage, Shapefile}
import org.openmole.spatialdata.vector.Polygons

import scala.util.Random

object RunMatsim extends App {
  implicit val rng: Random = new Random
  //if(args.length<2) throw new IllegalArgumentException("Must provide data file")
  //println("Data file: "+args(1))
  args(0) match {
    case "--network" =>
      println("Running road network construction for MATSim model in UK")
      // FUA names as arguments - for testing purpose, Manchester, London are on 4 OSRoads tile, Exeter also but smaller in terms of data
      // Exeter: SS,ST,SX,SY

      if(args.length!=4) throw new IllegalArgumentException("Missing arguments; usage: --network --FUAName=$NAME1,$NAME2,...,$NAMEN --FUAFile=$PATH --datadir=$DATADIR")

      val fuanames: Array[String] = args(1).split("=")(1).split(",")
      println("Constructing network for FUA: "+args(1))

      // load FUAs as polygons
      val fuapath: String = args(2).split("=")(1)
      val fualayername = fuapath.split("/").last.split(".")(0)
      val allfuas = GeoPackage.readGeometry(fuapath,featureName = fualayername)
      // name field for FUAs assumed as eFUA_name (JRC file) - add this as an option?
      val (fuasgeoms,fuasattrs) = allfuas.filter(f => fuanames.contains(f._2.getOrElse("eFUA_name",""))).unzip
      val fuas = Polygons.fromGeometries(fuasgeoms,fuasattrs)

      // load road data coverage
      // (note: specific to UK and split road dataset)
      val roaddatadir: String = args(3).split("=")(1)
      val (tilesgeoms,tilesattrs) = Shapefile.readGeometry(roaddatadir+"/OSOpenRoadsTiles.shp", Array("name")).unzip // tile layer name is hardcoded
      val tiles = Polygons.fromGeometries(tilesgeoms,tilesattrs)

      // find tiles with non empty intersection with FUAs
      fuas.polygons.map{p=>
        //tiles..filter()
        //p.intersects()
      }

      val nw = GISFileNetworkGenerator(args(1)).generateNetwork
      //println(nw)
      // export network to matsim format
      //Gexf.writeGexf(nw,"/data/outputs/network.gexf")
      MatsimNetworkGenerator.writeMatsimXML(nw, args(2))

    case "--synthpop" =>
      // convert spenser synth pop files to Matsim population

  }


}
