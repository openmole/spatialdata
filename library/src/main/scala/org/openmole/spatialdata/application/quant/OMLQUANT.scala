package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils.graph.GraphAlgorithms.JohnsonJGraphT
import org.openmole.spatialdata.utils.io.{CSV, GIS, GML}
import org.openmole.spatialdata.utils.osm.OSMPBFFile
import org.openmole.spatialdata.vector.Polygons

/**
 * Preliminary processing for integration of scenario Quant into OpenMOLE
 *
 *  1 - construct rail network (exported in gml)
 *    Rq: nodes are not proper stations, should add an other layer
 *
 *  2 - compute station to station shortest paths -> sparse
 *
 *  3 - compute sparse generalised distance matrix between MSOAs using closest station
 *
 *  4 - calibrate train speed using previous dmat (reconverted to be stored as generalised cost matrix - otherwise no sense in model computation)
 */
object OMLQUANT extends App {

  val osmFile: String = System.getenv("CS_HOME")+"/Data/OSM/Geofabrik/england/england_railways_20220627.pbf"
  val osmFileStations: String = System.getenv("CS_HOME")+"/Data/OSM/Geofabrik/england/england_stations_20220627.pbf"


  val gmlFile: String = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/processed/OSM_england_railways_20220627.gml"
  val dmatFile: String = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/processed/stations_dmat.csv"


  // 1 - construct rail network
  //Network.constructRailNetwork(osmFile, gmlFile)
  val (points, lines) = OSMPBFFile.readPBFFile(osmFile)
  utils.log(s"stations points: ${points.points.size} ; polylines: ${lines.lines.size}")

  //val network = GML.readGML(gmlFile)
  //utils.log(s"Initial nw: ${network.nodes.size} stations")

  // 2 - shortest paths
  //val shortestPaths = GraphAlgorithms.shortestPaths(network, from = network.nodes.toSeq, to = network.nodes.toSeq, linkWeight = _.length)
  //val nodes = network.nodes.toSeq
  //val shortestPaths = GraphAlgorithms.shortestPaths(network, from = nodes, to = nodes, linkWeight = _.length, JohnsonJGraphT())
  //utils.log(s"Average shortest path length = ${shortestPaths.values.map(_._3).sum / shortestPaths.size}")
  //CSV.writeCSV(data = nodes.map(n1 => nodes.map(n2 => shortestPaths((n1, n2))._3).toArray).toArray, file=dmatFile, sep = ",", header= nodes.map(_.id.toString).toArray)

  // closest MSOA
  //val msoas = Polygons(GIS.readGeometry(QUANT.MSOAFile, Array(QUANT.msoaID)))
  //utils.log(s"msoas: ${msoas.attributes.map(_.getOrElse("MSOA11CD", ""))}")



}
