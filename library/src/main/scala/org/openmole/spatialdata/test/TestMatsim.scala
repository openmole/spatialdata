package org.openmole.spatialdata.test

import org.openmole.spatialdata.network.real.MatsimNetworkGenerator
import org.openmole.spatialdata.utils.visualization

import scala.util.Random

object TestMatsim {

  implicit val rng: Random = new Random

  def testMatsimNetworkGenerator(): Unit = {
    val nw = Seq(MatsimNetworkGenerator("data/test/matsim.xml").generateNetwork.planarize)
    //val nw = Seq(MatsimNetworkGenerator(System.getenv("CS_HOME")+"/MobilityEpidemio/Models/Transportation/data/EpiSim/berlin-v5.5-network.xml").generateNetwork)
    //println(nw)
    visualization.staticNetworkVisualization(nw,nodePositioning = visualization.normalizedPosition(nw))
  }


}
