package org.openmole.spatialdata.test

import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.synthetic.RandomGridGenerator
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.network.synthetic.RandomNetworkGenerator
import org.openmole.spatialdata.utils.math.Statistics
import org.openmole.spatialdata.vector.synthetic.DensityPointsGenerator

import scala.util.Random

object TestCoupled {

  def testWeakCouplingNodePositions(gridSize:Int = 50, gridOccupation: Double = 0.5,nwNodes: Int = 25, nwLinks: Int = 250): Unit = {
    implicit val rng = new Random

    val randomGrid = RandomGridGenerator(gridSize,1.0,gridOccupation).generateGrid
    val points =  DensityPointsGenerator(nwNodes,randomGrid,true).generatePoints
    println(points)
    val randomNetwork = RandomNetworkGenerator(nlinks = nwLinks,points=points,planarize = true).generateNetwork
    println(randomNetwork.isPlanar)
    println(randomNetwork)

    println("Moran grid = "+GridMorphology.moran(randomGrid)+" ; Entropy = "+Statistics.entropy(randomGrid)+" ; slope = "+
      Statistics.slope(randomGrid)+" ; avg dist = "+GridMorphology.distanceMean(randomGrid)
    )
    //println(ShortestPathsNetworkMeasures(randomNetwork,pathSample = 0.05))
    println(NetworkMeasures(randomNetwork,pathSample = 1.0))

  }


  def testRandom(gridSize:Int = 50, gridOccupation: Double = 0.5,nwNodes: Int = 20, nwLinks: Int = 200): Unit = {

    implicit val rng = new Random

    val randomGrid = RandomGridGenerator(gridSize,1.0,gridOccupation).generateGrid
    val randomNetwork = RandomNetworkGenerator(nwNodes,nwLinks,true,false,false).generateNetwork

    println("Moran grid = "+GridMorphology.moran(randomGrid)+" ; Entropy = "+Statistics.entropy(randomGrid)+" ; slope = "+
    Statistics.slope(randomGrid)+" ; avg dist = "+GridMorphology.distanceMean(randomGrid)
    )
    println(NetworkMeasures(randomNetwork,1.0))

  }


}
