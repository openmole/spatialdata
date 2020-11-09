package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata.network.real.{GISFileNetworkGenerator, MatsimNetworkGenerator}
import org.openmole.spatialdata.utils.io.{GeoPackage, Shapefile}
import org.openmole.spatialdata.vector.{Attributes, Polygons}
import org.locationtech.jts.geom
import org.locationtech.jts.io.WKTWriter

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
      println("Constructing network for FUA: "+fuanames.mkString(","))

      // load FUAs as polygons
      val fuapath: String = args(2).split("=")(1)
      val fualayername = fuapath.split("/").last.split("\\.")(0)
      println("FUAs file path: "+fuapath+" ; layername: "+fualayername)
      val allfuas = GeoPackage.readGeometry(fuapath,featureName = fualayername, attributes = Array("eFUA_name"))
      // name field for FUAs assumed as eFUA_name (JRC file) - add this as an option?
      val fuas = allfuas.filter(f => fuanames.contains(f._2.getOrElse("eFUA_name","").asInstanceOf[String]))
      //val fuas = Polygons.fromGeometries(fuasgeoms,fuasattrs) // this fails as FUAs are multipolygons
      // if several FUAs, take the counding box to ensure a connected network, otherwise juste the polygon
      val area: geom.Geometry = if(fuas.size==1) fuas(0)._1 else Polygons(fuas.map(_._1.asInstanceOf[geom.Polygon])).getEnvelope
      println("Target network area: "+new WKTWriter().write(area))

      // load road data coverage
      // (note: specific to UK and split road dataset)
      val roaddatadir: String = args(3).split("=")(1)
      val (tilesgeoms,tilesattrs) = Shapefile.readGeometry(roaddatadir+"/OSOpenRoadsTiles.shp", Array("name")).unzip // tile layer name is hardcoded
      val tiles = Polygons.fromGeometries(tilesgeoms,tilesattrs)
      println("Map tiles: "+tiles.polygons.size)

      // find tiles with non empty intersection with FUAs
      val reqtiles: Seq[(geom.Polygon,Attributes)] = tiles.polygons.zip(tiles.attributes).filter(_._1.intersects(area))
      println("Requested tiles names are: "+reqtiles.map(_._2.get("name")).mkString(","))

      //val nw = GISFileNetworkGenerator(args(1)).generateNetwork
      //println(nw)
      // export network to matsim format
      //Gexf.writeGexf(nw,"/data/outputs/network.gexf")
      //MatsimNetworkGenerator.writeMatsimXML(nw, args(2))

    case "--synthpop" =>
      // convert spenser synth pop files to Matsim population

  }


}
