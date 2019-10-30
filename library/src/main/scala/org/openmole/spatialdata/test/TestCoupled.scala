package org.openmole.spatialdata.test

import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.synthetic.RandomGridGenerator
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.network.synthetic.RandomNetworkGenerator
import org.openmole.spatialdata.utils.math.Statistics

import scala.util.Random


object TestCoupled {


  def testRandom(gridSize:Int = 50, gridOccupation: Double = 0.5,nwNodes: Int = 20, nwLinks: Int = 200): Unit = {

    implicit val rng = new Random

    val randomGrid = RandomGridGenerator(gridSize,1.0,gridOccupation).generateGrid
    val randomNetwork = RandomNetworkGenerator(nwNodes,nwLinks,true,false,false).generateNetwork

    println("Moran grid = "+GridMorphology.moran(randomGrid)+" ; Entropy = "+Statistics.entropy(randomGrid)+" ; slope = "+
    Statistics.slope(randomGrid)+" ; avg dist = "+GridMorphology.distanceMean(randomGrid)
    )
    println(NetworkMeasures(randomNetwork))

  }


}
