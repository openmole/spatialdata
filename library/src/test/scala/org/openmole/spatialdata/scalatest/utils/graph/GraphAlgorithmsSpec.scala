package org.openmole.spatialdata.scalatest.utils.graph

import org.openmole.spatialdata.network.{Network, ShortestPaths}
import org.openmole.spatialdata.network.synthetic.{RandomNetworkGenerator, TreeMinDistGenerator}
import org.openmole.spatialdata.utils.graph.GraphAlgorithms._
import org.openmole.spatialdata.utils.withTimer
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class GraphAlgorithmsSpec extends AnyFlatSpec {


  implicit val rng = new Random


  //ignore
  "Shortest paths implementations" should "yield the same results" in {
    //val nwgen = RandomNetworkGenerator(10,20,true,false,false)
    val nwgen = RandomNetworkGenerator(5,10,true,false,false)

    (0 until 10).foreach { _ =>
      val nw = nwgen.generateNetwork
      val nodes = nw.nodes.toSeq
      val (sp1, t1) = withTimer[Network, ShortestPaths](nw => shortestPaths(nw, nodes, { l => l.length }, ScalaGraph()))(nw)
      val (sp2, t2) = withTimer[Network, ShortestPaths](nw => shortestPaths(nw, nodes, { l => l.length }, DijkstraJGraphT()))(nw)
      val (sp3, t3) = withTimer[Network, ShortestPaths](nw => shortestPaths(nw, nodes, { l => l.length }, FloydWarshallJGraphT()))(nw)
      assert((sp1 |-| sp2) < 1e-5 && (sp1 |-| sp3) < 1e-5 && (sp3 |-| sp2) < 1e-5)
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


}
