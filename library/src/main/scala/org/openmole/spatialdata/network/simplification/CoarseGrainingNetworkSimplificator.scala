package org.openmole.spatialdata.network.simplification

import org.locationtech.jts.geom
import org.locationtech.jts.geom.Envelope
import org.openmole.spatialdata.network.{Link, Network, NetworkSimplificator, Node}
import org.openmole.spatialdata.vector.Polygons
import org.locationtech.jts.index.quadtree.Quadtree
import org.openmole.spatialdata.utils

import scala.jdk.CollectionConverters._

/**
  * Network coarse graining
  *
  * @param layer simplification polygons
  */
case class CoarseGrainingNetworkSimplificator(
                                               layer: Polygons
                                             ) extends NetworkSimplificator {

  override def simplifyNetwork(network: Network): Network = CoarseGrainingNetworkSimplificator.coarseGrainNetwork(network,layer)._1

}



object CoarseGrainingNetworkSimplificator {


  /**
    * Coarse grain a network given a polygon layer
    *
    *  * for now times are computed from network nodes: add the option of an origin layer (possibly weighted e.g. population)
    *  * internal times should be optional (to avoid overhead computational cost when not used)
    *
    * @param network network
    * @param layer polygon layer over which coarse graining is done
    * @param connectorWeight function giving the weight of connector links (centroid to closest node): typically, walking or driving speed
    * @return the simplified network and average path length within each polygon
    */
  def coarseGrainNetwork(network: Network, layer: Polygons, connectorWeight: Double => Double = {d: Double => d}): (Network, Seq[Double]) = {

    val (nodes,nodepoints) = network.nodesAsPoints
    val pointNodeMap: Map[geom.Point,Node] = nodepoints.points.zip(nodes).toMap
    val nodePointMap: Map[Node, geom.Point] = nodes.zip(nodepoints.points).toMap

    val maxEnvWidth = layer.polygons.map(p => p.getEnvelopeInternal.getWidth).max(Ordering.Double.TotalOrdering)
    val maxEnvHeight = layer.polygons.map(p => p.getEnvelopeInternal.getHeight).max(Ordering.Double.TotalOrdering)

    // use a spatial index for efficiency: add nw nodes / polygon centroids? / polygons?
    utils.log(s"Constructing quadtree spatial index for ${layer.polygons.size} polygons and ${nodepoints.points.size} points")
    val spatialIndex: Quadtree = new Quadtree
    layer.polygons.foreach(p => spatialIndex.insert(p.getEnvelopeInternal,p))
    nodepoints.points.foreach(p => spatialIndex.insert(p.getEnvelopeInternal,p))

    // isolate extended subnetwork for each polygon (with neighbors and strict), compute also the node closest to the centroid
    // note: all created subnetworks are compatible in terms of node ids
    val polygonSubNetworks: Map[geom.Polygon, (Network, Network, Node)] = layer.polygons.map{p =>
      val withinnodes = spatialIndex.
        query(p.getEnvelopeInternal).asScala. // query spatial index
        filter(_.isInstanceOf[geom.Point]). // other elements in index are polygons
        map(_.asInstanceOf[geom.Point]).
        filter(p.contains(_)). // get point effectively in the polygon
        map(pointNodeMap(_))
      if (withinnodes.isEmpty) None
      else {
        // get node closest to the centroid
        // ! assumption: time to centroid is time to this node + connector time (may not be true in some cases)
        // for better accuracy, should add several connectors (at least to each enclosing node) and include in shortest paths
        val centroid = p.getCentroid
        val ncentroid = withinnodes.map(n => (nodePointMap(n).distance(centroid), n)).sortBy(_._1)(Ordering.Double.TotalOrdering).head._2
        Some((p, (network.subNetworkNodes(withinnodes.toSet, withNeighbors = true), network.subNetworkNodes(withinnodes.toSet), ncentroid)))
      }
    }.filter(_.isDefined).map(_.get).toMap

    // find other polygons directly linked to a given one with subnetwork links
    // rq: introduces a bias, if a polygon is very close but not directly linked, then when computing shortest paths to centroids only,
    // may do a big detour through the centroid of the intermediate polygon.
    // could add a buffer radius to avoid this
    def getNeighbors(p: geom.Polygon): Seq[geom.Polygon] = {
      val (currentnw,_,_) = polygonSubNetworks(p)
      val outsidepoints = currentnw.nodes.map(nodePointMap(_)).filter(!p.contains(_))
      // find polygons in which each outside point falls using the spatial index
      outsidepoints.map { point =>
        val candidatePolygons = spatialIndex.query(new Envelope(point.getCoordinate.x-maxEnvWidth, point.getCoordinate.x+maxEnvWidth,
          point.getCoordinate.y-maxEnvHeight, point.getCoordinate.y+maxEnvHeight)). // will coincide with max polygon envelope if point is exactly on the border - should be marginal
          asScala.filter(_.isInstanceOf[geom.Polygon]).map(_.asInstanceOf[geom.Polygon])
        candidatePolygons.filter(_.contains(point)).toSeq // filter for polygons effectively containing the point - rq: the algo is conceived for partition of space, it should yield one polygon
      }.toSeq.flatten
    }

    // main step of the algo: for each subnetwork, compute internal average travel time to centroid, and travel time to centroid of each neighbor
    // avg travel time between nodes makes sense in case of a dense nw: should be able to add an other layer of O/D point
    // (typically center of population raster, weighted by population)
    val polyLinks: Seq[(geom.Polygon,Double, Seq[(geom.Polygon, Double)])] = polygonSubNetworks.map{e: (geom.Polygon,(Network,Network,Node)) =>
      val (p,(nneigh,nwithin,c)) = e
      // average dist to centroid: connector time added at this step
      //println(s"internal shortest paths: destination $c")
      //println(s"    nwithin contains dest: ${nwithin.nodes.contains(c)} ; nneigh contains dest: ${nneigh.nodes.contains(c)}")
      //  issue when target node not in network ? ok - pb with nw conversion
      val avgdistwithin = (nwithin.shortestPathTo(nwithin.nodes.toSeq,Seq(c)).toSeq.filter(_._2._3.isFinite).map(_._2._3).sum / nwithin.nodes.size) +
        connectorWeight(p.getCentroid.distance(nodePointMap(c)))
      val neighbors = getNeighbors(p)
      val neighlinks = neighbors.map{neigh =>
        val (_,neighwithin,cneigh) = polygonSubNetworks(neigh)

        // superpose the networks: by construction both are connected - otherwise infinity
        //  subnetwork is not necessarily connected itself: most of it may not find a path to neighbor centroid this way (if most path go through another neighbor)
        //  -> considering the full network will have a high computational cost
        //  -> considering all neighbors may also miss very detoured paths
        //  -> add connectors to the local network may be a good alternative (rq: a non-connected local nw makes no sense in practice) (-> do not test on trees)
        // : this should be an option
        //println(s"shortest path from $c to $cneigh")
        //   issue when target node is NOT in the network - ok
        //   issue with selfloops (same node to same node): local loop nw issue? - ok fixed nw generator
        // Note: in current setting, if centroid is disconnected, some links may have infinite weights !
        // Rq: given the speed, could use all network? maybe tricky for all UK
        // + manage connected components?
        val d = (nneigh+neighwithin).shortestPathTo(Seq(c),Seq(cneigh)).head._2._3 +
          connectorWeight(p.getCentroid.distance(nodePointMap(c))) + connectorWeight(neigh.getCentroid.distance(nodePointMap(cneigh)))
        (neigh,d)
      }
      (p,avgdistwithin,neighlinks)
    }.toSeq

    // finally construct the final network. All polygons which can be neighbors (with nodes) are in the first set, used for nodes
    val finalNodes = polyLinks.zipWithIndex.map{case ((p,_,_),i) => val coord = p.getCentroid.getCoordinate; (p,Node(i,coord.x,coord.y))}.toMap
    val links = polyLinks.flatMap{case (p,_,neigh) => neigh.map{case (n,d) => Link(finalNodes(p),finalNodes(n),weight=d)}}.toSet
    (Network(nodes = finalNodes.values.toSet, links = links),polyLinks.map(_._2))
  }


}
