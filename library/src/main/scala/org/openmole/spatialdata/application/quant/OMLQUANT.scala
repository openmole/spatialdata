package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.network.Link
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils.graph.GraphAlgorithms.JohnsonJGraphT
import org.openmole.spatialdata.utils.io.{CSV, GIS, GML, GeoPackage}
import org.openmole.spatialdata.utils.osm.OSMPBFFile
import org.openmole.spatialdata.vector.{Points, Polygons}

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
  val dmatFileScenario: String = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/processed/stations_dmat_scenario.csv"


  // 1 - construct rail network
  //val (points, lines) = OSMPBFFile.readPBFFile(osmFileStations)
  //utils.log(s"stations points: ${points.points.size} ; polylines: ${lines.lines.size}")
  //utils.log(s"stations coords: [${points.points.map(_.getX).min} - ${points.points.map(_.getX).max}] x [${points.points.map(_.getY).min} - ${points.points.map(_.getY).max}]")
  //utils.log(s"${points.points.map(p=> (p.getX, p.getY))}")
  //Network.constructRailNetwork(osmFile, osmFileStations, gmlFile)

  val network = GML.readGML(gmlFile, Seq("station"))
  utils.log(s"Initial nw: ${network.nodes.count(_.attributes.contains("station"))} stations")
  //val nodes = network.nodes
  //GeoPackage.writeGeometry(Points.fromPoints(nodes.map(n => (n.x, n.y))).asGeometryCollection, "network" ,gmlFile+".gpkg",Map("id" -> nodes.toSeq.map(_.id.asInstanceOf[AnyRef])))

  // 2 - shortest paths
  //val stations = network.nodes.toSeq.filter(_.attributes.contains("station"))
  //val shortestPaths = GraphAlgorithms.shortestPaths(network, from = stations, to = stations, linkWeight = _.length)
  //val nodes = network.nodes.toSeq
  //val shortestPaths = GraphAlgorithms.shortestPaths(network, from = nodes, to = nodes, linkWeight = _.length, JohnsonJGraphT())
  //utils.log(s"Average shortest path length = ${shortestPaths.values.map(_._3).sum / shortestPaths.size}")
  //CSV.writeCSV(data = stations.map(n1 => stations.map(n2 => shortestPaths((n1, n2))._3).toArray).toArray, file=dmatFile, sep = ",", header= stations.map(_.id.toString).toArray)


  // 3 - Camcox scenario
  // by hand see camcox.shp layer
  // 41676 -- 3482 --37557 -- 14858 -- 2229 -- 27774 -- 16134
  def link(id1: Int, id2: Int): Link = Link(network.node(id1), network.node(id2))
  val scenarionw = network.addLinks(Set(link(41676,3482), link(3482, 37557), link(37557, 14858), link(14858, 2229), link(2229, 27774), link(27774, 16134)))
  val stations = scenarionw.nodes.toSeq.filter(_.attributes.contains("station"))
  val shortestPaths = GraphAlgorithms.shortestPaths(scenarionw, from = stations, to = stations, linkWeight = _.length)
  CSV.writeCSV(data = stations.map(n1 => stations.map(n2 => shortestPaths((n1, n2))._3).toArray).toArray, file=dmatFileScenario, sep = ",", header= stations.map(_.id.toString).toArray)

  // 4 - MSOA distance matrix
  //val dmatstations = CSV.readMat(dmatFile)
  // closest MSOA
  //val msoas = Polygons(GIS.readGeometry(QUANT.MSOAFile, Array(QUANT.msoaID)))
  // order them in same order than Quant matrices -> need the csv file
  //utils.log(s"msoas: ${msoas.attributes.map(_.getOrElse("MSOA11CD", ""))}")
  //val msoacent: Seq[Point] = msoas.polygons.map(_.getCentroid)



}
