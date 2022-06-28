package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.network.Node
import org.openmole.spatialdata.network.simplification.WayNetworkSimplificator
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.gis.GISNetwork
import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils.graph.GraphAlgorithms.ConnectedComponentsJGraphT
import org.openmole.spatialdata.utils.io.{GIS, GeoPackage}
import org.openmole.spatialdata.utils.math.SparseMatrix
import org.openmole.spatialdata.utils.osm.OSMPBFFile
import org.openmole.spatialdata.utils.visualization
import org.openmole.spatialdata.vector.{Point, Points, Polygons}

import scala.collection.mutable

object Network {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  /**
   * Computes distance matrices for a rail network and a given scenario on it.
   *
   *  - Network is computed from an OSM pbf file -> first filter with osmosis
   *     cant use the OSMNetworkGenerator as network is country-wide
   *
   * @return
   */
  def railNetworkScenarioOSM: (SparseMatrix, SparseMatrix) = {

    // add MSOA centroid layer (then scenario between MSOAs (closest station to each centroid?)) // ! missing file: not on disk?
    //val msoas = Polygons(GIS.readGeometry(QUANT.MSOAFile, Array(QUANT.msoaID)))

    val dummy = SparseMatrix.randomSparseMatrix(0,0, 0.1)(new scala.util.Random, SparseMatrix.SparseCommons())
    (dummy, dummy)
  }

  /**
   *
   * @param osmFile OSM file

   */
  def constructRailNetwork(osmFile: String, osmFileStations: String, gmlFile: String): Unit = {
    // construct rail network from OSM
    val (_, lines) = OSMPBFFile.readPBFFile(osmFile)
    val nw = WayNetworkSimplificator({case (l1,l2)=>l1.length+l2.length}, {case (l1,l2)=>(l1.weight*l1.length + l2.weight*l2.length)/(l1.length+l2.length)}).
      simplifyNetwork(GISNetwork.networkFromGISLinesGridSnapping(lines, snap = 0.001))

    // for data check
    GeoPackage.writeGeometry(Points.fromPoints(nw.nodes.map(n => (n.x, n.y))).asGeometryCollection, "network" ,gmlFile+".gpkg",Map())

    // keep largest weak component
    val comps = GraphAlgorithms.connectedComponents(nw, ConnectedComponentsJGraphT())
    val largestCompSize = comps.map(_.nodes.size).max
    val lnw = comps.filter(_.nodes.size == largestCompSize).head


    // add stations
    val (stations, _) = OSMPBFFile.readPBFFile(osmFileStations)

    val newnodes = new mutable.HashMap[Node, Node]
    val nodes = lnw.nodes.toSeq
    stations.points.foreach{s =>
      val dists = nodes.map(n =>  math.sqrt(math.pow(n.x-s.getX,2)+math.pow(n.y-s.getY,2)))
      //utils.log(s"${dists}")
      val closestNode = dists.zip(nodes).minBy(_._1)
      //utils.log(s"(${s.getX}, ${s.getY}) -> ${closestNode}")
      newnodes.put(closestNode._2, closestNode._2.copy(attributes = Map("station" -> "true")))
    }
    //utils.log(s"${newnodes.keys.toSeq}")
    val finalNodes = nodes.map(n => newnodes.getOrElse(n, n))
    val finalnw = lnw.copy(nodes = finalNodes.toSet)
    utils.log(s"Final rail network with ${finalnw.nodes.size} nodes and ${finalnw.nodes.count(_.attributes.contains("station"))} stations")

    utils.io.GML.writeGML(finalnw, gmlFile)

    //val (xmin,xmax,ymin,ymax) = (lnw.nodes.map{_.x}.min,lnw.nodes.map{_.x}.max,lnw.nodes.map{_.y}.min,lnw.nodes.map{_.y}.max)
    //def position(n: Node): Point = ((n.x - xmin)/(xmax-xmin),(n.y - ymin)/(ymax-ymin))
    //visualization.staticNetworkVisualization(Seq(lnw), nodePositioning = position)

  }


}
