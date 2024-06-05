package org.openmole.spatialdata.application.matsim

import org.locationtech.jts.geom
import org.locationtech.jts.io.WKTWriter
import org.openmole.spatialdata.{network, utils}
import org.openmole.spatialdata.network.real.{GISFileNetworkGenerator, MatsimNetworkGenerator}
import org.openmole.spatialdata.utils.gis.GeometryUtils
import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils.graph.GraphAlgorithms.ConnectedComponentsJGraphT
import org.openmole.spatialdata.utils.io.Shapefile
import org.openmole.spatialdata.vector.{Attributes, Lines, Polygons}

import scala.util.Random

object Network {

  implicit val rng: Random = new Random

  /**
    * Construct networks for given areas
    *
    * FUA names as arguments - for testing purpose, Manchester, London are on 4 OSRoads tile, Exeter also but smaller in terms of data
    *
    *   Exeter test command (Exeter tiles: SS,ST,SX,SY)
    *     run --network --FUAName=Exeter --FUAFile=CS_HOME/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg --TilesFile=CS_HOME/UrbanDynamics/Models/Matsim/Network/data/OSOpenRoadsTiles.shp --datadir=CS_HOME/UrbanDynamics/Data/OrdnanceSurvey/OSOpenRoads --output=CS_HOME/UrbanDynamics/Data/OrdnanceSurvey/OSOpenRoads/Exeter_Roads.xml
    *
    *   Taunton test
    *     runMain org.openmole.spatialdata.application.matsim.RunMatsim --network --FUAName="Taunton" --FUAFile=/Users/juste/ComplexSystems/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg --TilesFile=/Users/juste/ComplexSystems/UrbanDynamics/Models/Matsim/Network/data/OSOpenRoadsTiles.shp --datadir=/Users/juste/ComplexSystems/UrbanDynamics/Data/OrdnanceSurvey/OSOpenRoads --output=/Users/juste/ComplexSystems/UrbanDynamics/Models/Matsim/test/Network
    *
    *   Test London: SP,SU,TL,TQ
    *
    *   Test several (small multi-FUAs areas): Taunton,Weston-super-Mare;Exeter,Torquay ... --output=CS_HOME/UrbanDynamics/Data/OrdnanceSurvey/OSOpenRoads/Exeter_Roads.xml
    *
    * @param args arguments; usage: --network --FUAName="NAME1,NAME2,...,NAME_N;...,...;..." --FUAFile=PATH --TilesFile=PATHTILES --datadir=DATADIR --output=OUTPUT
    */
  def runNetworkProcessing(args: Array[String]): Unit = {

    println("Running road network construction for MATSim model in UK")

    if(args.length!=6) throw new IllegalArgumentException("Missing arguments; usage:\n" +
      "--network\n" +
      "--FUAName=\"$NAME1,$NAME2,...,$NAMEN\"\n" +
      "--FUAFile=$PATH\n" +
      "--TilesFile=$PATHTILES\n" +
      "--datadir=$DATADIR\n" +
      "--output=$OUTPUT")

    val fuanames= Matsim.parseArg(args, "FUAName").replace("\"","").split(";").toSeq
    utils.log("Constructing network for FUAs: "+fuanames.mkString("  ;  "))
    val fuapath=Matsim.parseArg(args, "FUAFile")
    val areas = Matsim.loadAreas(fuanames.map(_.split(",").toSeq), fuapath)
    utils.log("Target network areas: "+areas.map{area=>new WKTWriter().write(area.getEnvelope)}.mkString("\n"))

    // load road data coverage
    // (note: specific to UK and split road dataset)
    val tilesfile = Matsim.parseArg(args, "TilesFile")
    val (tilesgeoms,tilesattrs) = Shapefile.readGeometry(tilesfile, Array("name")).unzip // tile layer name is hardcoded
    val tiles = Polygons.fromGeometries(tilesgeoms,tilesattrs)
    utils.log("Map tiles: "+tiles.polygons.size)

    val roaddatadir: String = Matsim.parseArg(args, "datadir")

    val networks = areas.map(area => constructNetwork(area, tiles, roaddatadir))

    // export network to matsim format
    val output = Matsim.parseArg(args, "output")
    utils.log("Exporting network to file prefix "+output)

    networks.zip(fuanames).foreach {case (nw,name) =>
      utils.log("Area: "+name+": |V|="+nw.nodes.size+"; |E|="+nw.links.size)
      MatsimNetworkGenerator.writeMatsimXML(nw, output+"_"+name.replace(",","-")+".xml")
    }
  }

  /**
    * Road network construction from area, tile using a GISFileNetworkGenerator
    * @param area area
    * @param tiles tiles
    * @param roaddatadir directory for tile files
    * @param sourceCRS if reprojection needed, source CRS (default to UK: EPSG:27700)
    * @param targetCRS target CRS
    * @return
    */
  def constructNetwork(area: geom.Geometry, tiles: Polygons, roaddatadir: String, sourceCRS: String = "EPSG:27700", targetCRS: String = "EPSG:4326"): network.Network = {
    // find tiles with non empty intersection with FUAs
    val reqtiles: Seq[(geom.Polygon,Attributes)] = tiles.polygons.zip(tiles.attributes).filter(_._1.intersects(area))
    val tilenames = reqtiles.map(_._2.getOrElse("name",""))
    // if no tile available: empty network!
    if (tilenames.isEmpty) return network.Network.empty

    utils.log("Requested tiles names are: "+tilenames.mkString(","))

    // construct network - ! OS files do not have speed attribute?
    // why are coordinates translated? issue with shp vs geopkg?
    val mask: Option[Either[geom.Geometry,String]] = Some(Left(area))
    val reproject: Option[Lines => Lines] = if(sourceCRS.contains("EPSG")) Some({
      (lines: Lines) =>
        val reproj = lines.transform(sourceCRS,targetCRS)
        //utils.log("Before tr: "+reproj.lines.take(2).map(_.toString).mkString("\n"))
        val trlines: Lines = Lines(reproj.lines.map(GeometryUtils.transpose(_).asInstanceOf[geom.LineString]),lines.attributes)
        //utils.log("Transposed: "+trlines.lines.take(2).map(_.toString).mkString("\n"))
        trlines
    }) else None
    val nw = GISFileNetworkGenerator(tilenames.map{s => roaddatadir+"/"+s+"_RoadLink.shp"}, mask = mask, reproject=reproject).generateNetwork
    utils.log("Network size: |V| = "+nw.nodes.size+"; |E| = "+nw.links.size)
    utils.log("Duplicate ids: max nodes per id = "+nw.nodes.groupBy(_.id).values.map(_.size).max)
    utils.log("Connected components : "+GraphAlgorithms.connectedComponents(nw,method = ConnectedComponentsJGraphT()).size)
    //nw.weakComponentConnect // algo is inefficient
    val largestcomponent = GraphAlgorithms.largestConnectedComponent(nw, method = ConnectedComponentsJGraphT())
    utils.log("Final network size: |V| = "+largestcomponent.nodes.size+"; |E| = "+largestcomponent.links.size)
    largestcomponent
  }


}
