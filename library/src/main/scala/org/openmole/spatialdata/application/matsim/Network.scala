package org.openmole.spatialdata.application.matsim

import org.locationtech.jts.geom
import org.locationtech.jts.io.WKTWriter
import org.openmole.spatialdata.network.real.{GISFileNetworkGenerator, MatsimNetworkGenerator}
import org.openmole.spatialdata.utils.gis.GeometryUtils
import org.openmole.spatialdata.utils.io.{GeoPackage, Shapefile}
import org.openmole.spatialdata.vector.{Attributes, Lines, Polygons}

import scala.util.Random

object Network {

  implicit val rng: Random = new Random

  def runNetworkProcessing(args: Array[String]): Unit = {

    println("Running road network construction for MATSim model in UK")
    // FUA names as arguments - for testing purpose, Manchester, London are on 4 OSRoads tile, Exeter also but smaller in terms of data
    // Exeter: SS,ST,SX,SY
    // Exeter test command
    // run --network --FUAName=Exeter --FUAFile=/Users/juste/ComplexSystems/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg --datadir=/Users/juste/ComplexSystems/UrbanDynamics/Data/OrdnanceSurvey/OSOpenRoads --output=/Users/juste/ComplexSystems/UrbanDynamics/Data/OrdnanceSurvey/OSOpenRoads/Exeter_Roads.xml
    // Test London: SP,SU,TL,TQ


    if(args.length!=6) throw new IllegalArgumentException("Missing arguments; usage: --network --FUAName=$NAME1,$NAME2,...,$NAMEN --FUAFile=$PATH --TilesFile=$PATHTILES --datadir=$DATADIR --output=$OUTPUT")

    val fuanames= Matsim.parseArg(args, "FUAName").split(",")
    println("Constructing network for FUA: "+fuanames.mkString(","))
    val fuapath=Matsim.parseArg(args, "FUAFile")
    val area = Matsim.loadArea(fuanames, fuapath)
    println("Target network area: "+new WKTWriter().write(area))

    // load road data coverage
    // (note: specific to UK and split road dataset)
    val tilesfile = Matsim.parseArg(args, "TilesFile")
    val (tilesgeoms,tilesattrs) = Shapefile.readGeometry(tilesfile, Array("name")).unzip // tile layer name is hardcoded
    val tiles = Polygons.fromGeometries(tilesgeoms,tilesattrs)
    println("Map tiles: "+tiles.polygons.size)

    // find tiles with non empty intersection with FUAs
    val reqtiles: Seq[(geom.Polygon,Attributes)] = tiles.polygons.zip(tiles.attributes).filter(_._1.intersects(area))
    val tilenames = reqtiles.map(_._2.getOrElse("name",""))
    println("Requested tiles names are: "+tilenames.mkString(","))

    // construct network - ! OS files do not have speed attribute?
    // why are coordinates translated? issue with shp vs geopkg?
    val roaddatadir: String = Matsim.parseArg(args, "datadir")
    val mask: Option[Either[geom.Geometry,String]] = Some(Left(area))
    val reproject: Option[Lines => Lines] = Some({
      lines: Lines =>
        val reproj = lines.transform("EPSG:27700","EPSG:4326")
        //utils.log("Before tr: "+reproj.lines.take(2).map(_.toString).mkString("\n"))
        val trlines: Lines = Lines(reproj.lines.map(GeometryUtils.transpose(_).asInstanceOf[geom.LineString]),lines.attributes)
        //utils.log("Transposed: "+trlines.lines.take(2).map(_.toString).mkString("\n"))
        trlines
    })
    val nw = GISFileNetworkGenerator(tilenames.map{s => roaddatadir+"/"+s+"_RoadLink.shp"}, weightAttribute = "", mask = mask, reproject=reproject).generateNetwork
    println("Network size: |V| = "+nw.nodes.size+"; |E| = "+nw.links.size)

    // export network to matsim format
    val output = Matsim.parseArg(args, "output")
    println("Exporting network to file "+output)
    MatsimNetworkGenerator.writeMatsimXML(nw, output)
  }


}
