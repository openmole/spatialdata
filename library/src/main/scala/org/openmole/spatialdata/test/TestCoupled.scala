package org.openmole.spatialdata.test

import org.openmole.spatialdata.grid.Implicits._
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.synthetic.RandomGridGenerator
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.network.synthetic.RandomNetworkGenerator
import org.openmole.spatialdata.utils.math.Statistics
import org.openmole.spatialdata.vector.synthetic.DensityPointsGenerator

import scala.util.Random

object TestCoupled {

  def testWeakCouplingNodePositions(gridSize:Int = 50, gridOccupation: Double = 0.5,nwNodes: Int = 25, nwLinks: Int = 250, quiet: Boolean = false): Unit = {
    implicit val rng: Random = new Random

    val randomGrid = RandomGridGenerator(gridSize,1.0,gridOccupation).generateGrid
    val points =  DensityPointsGenerator(nwNodes,randomGrid,normalize = true).generatePoints.asPointSeq
    if (!quiet) println(points)
    val randomNetwork = RandomNetworkGenerator(nlinks = nwLinks,points=points,planarize = true).generateNetwork

    val (moran,entropy,slope,dmean) = (GridMorphology.moran(randomGrid),Statistics.entropy(randomGrid),Statistics.slope(randomGrid),GridMorphology.distanceMean(randomGrid))
    val nwmeasures = NetworkMeasures(randomNetwork, pathSample = 1.0)

    if (!quiet) {
      println(randomNetwork.isPlanar)
      println(randomNetwork)

      println("Moran grid = " + moran + " ; Entropy = " + entropy + " ; slope = " +
        slope + " ; avg dist = " + dmean
      )
      //println(ShortestPathsNetworkMeasures(randomNetwork,pathSample = 0.05))
      println(nwmeasures)
    }

  }


  def testRandom(gridSize:Int = 50, gridOccupation: Double = 0.5,nwNodes: Int = 20, nwLinks: Int = 200, quiet: Boolean = false): Unit = {

    implicit val rng: Random = new Random

    val randomGrid = RandomGridGenerator(gridSize,1.0,gridOccupation).generateGrid
    val randomNetwork = RandomNetworkGenerator(nwNodes,nwLinks,planarize = true,withIndex = false).generateNetwork

    val (moran,entropy,slope,dmean) = (GridMorphology.moran(randomGrid),Statistics.entropy(randomGrid),Statistics.slope(randomGrid),GridMorphology.distanceMean(randomGrid))
    val nwmeasures = NetworkMeasures(randomNetwork, pathSample = 1.0)

    if (!quiet) {
      println("Moran grid = " + moran + " ; Entropy = " + entropy + " ; slope = " +
        slope + " ; avg dist = " + dmean
      )
      println(nwmeasures)
    }

  }


}
