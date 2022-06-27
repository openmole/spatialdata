package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.network.Node
import org.openmole.spatialdata.network.simplification.WayNetworkSimplificator
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.gis.GISNetwork
import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils.io.GIS
import org.openmole.spatialdata.utils.math.SparseMatrix
import org.openmole.spatialdata.utils.osm.OSMPBFFile
import org.openmole.spatialdata.utils.visualization
import org.openmole.spatialdata.vector.{Point, Polygons}

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
  def constructRailNetwork(osmFile: String, gmlFile: String): Unit = {
    // construct rail network from OSM
    val (_, lines) = OSMPBFFile.readPBFFile(osmFile)
    val nw = WayNetworkSimplificator({case (l1,l2)=>l1.length+l2.length}, {case (l1,l2)=>(l1.weight*l1.length + l2.weight*l2.length)/(l1.length+l2.length)}).
      simplifyNetwork(GISNetwork.networkFromGISLinesGridSnapping(lines, snap = 0.001))
    // keep largest weak component
    val comps = GraphAlgorithms.connectedComponents(nw)
    val largestCompSize = comps.map(_.nodes.size).max
    val lnw = comps.filter(_.nodes.size == largestCompSize).head

    utils.io.GML.writeGML(lnw, gmlFile)

    val (xmin,xmax,ymin,ymax) = (lnw.nodes.map{_.x}.min,lnw.nodes.map{_.x}.max,lnw.nodes.map{_.y}.min,lnw.nodes.map{_.y}.max)
    def position(n: Node): Point = ((n.x - xmin)/(xmax-xmin),(n.y - ymin)/(ymax-ymin))
    visualization.staticNetworkVisualization(Seq(lnw), nodePositioning = position)

  }


}
