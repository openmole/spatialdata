package org.openmole.spatialdata.scalatest.utils.graph

import org.openmole.spatialdata.network.{Network, ShortestPaths}
import org.openmole.spatialdata.network.synthetic.{RandomNetworkGenerator, TreeMinDistGenerator}
import org.openmole.spatialdata.utils.graph.GraphAlgorithms._
import org.openmole.spatialdata.utils.withTimer
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class GraphAlgorithmsSpec extends AnyFlatSpec {


  implicit val rng: Random = new Random


  //ignore
  "Shortest paths implementations" should "yield the same results" in {
    //val nwgen = RandomNetworkGenerator(10,20,true,false,false)
    val nwgen = RandomNetworkGenerator(5,10,planarize = true,directed = false,withIndex = false)

    (0 until 10).foreach { _ =>
      val nw = nwgen.generateNetwork
      val nodes = nw.nodes.toSeq
      val (sp2, _) = withTimer[Network, ShortestPaths](nw => shortestPaths(nw, nodes, nodes , { l => l.length }, DijkstraJGraphT()))(nw)
      val (sp3, _) = withTimer[Network, ShortestPaths](nw => shortestPaths(nw, nodes, nodes, { l => l.length }, FloydWarshallJGraphT()))(nw)
      assert((sp3 |-| sp2) < 1e-5)
    }
  }

  "Weak component implementations" should "yield the same results" in {
    val nwgen = TreeMinDistGenerator(200)
    (0 until 10).foreach { _ =>
      val nw = nwgen.generateNetwork.removeRandomLinks(50)
      val comps1 = connectedComponents(nw,ConnectedComponentsTraverse())
      val comps2 = connectedComponents(nw,ConnectedComponentsJGraphT())
      // comps may be in any order?
      val intersizes = for {
        comp1 <- comps1
        comp2 <- comps2
        // compare intersecting components: if same set, set diff of intersecting should all be empty
        if comp1.nodes.intersect(comp2.nodes).nonEmpty
        diffsize = comp1.nodes.diff(comp2.nodes).size
      } yield diffsize
      //println(intersizes)
      assert(intersizes.sum==0)
    }
  }

  /*
  "A simplified graph" should "be smaller" in {
    val (lat,lon) = (51.5213835,-0.1347904)
    val nw = OSMNetworkGenerator(lon,lat,5000,simplifySnapping = 0.02).generateNetwork
    val (xmin,xmax,ymin,ymax) = (nw.nodes.map{_.x}.min,nw.nodes.map{_.x}.max,nw.nodes.map{_.y}.min,nw.nodes.map{_.y}.max)
    def position(n: Node): Point = ((n.x - xmin)/(xmax-xmin),(n.y - ymin)/(ymax-ymin))
    val simplified = GraphAlgorithms.SimplificationAlgorithm.simplifyNetwork(nw)
    assert(nw.nodes.size>=simplified.nodes.size&&nw.links.size>=simplified.links.size)
  }
  */



}
