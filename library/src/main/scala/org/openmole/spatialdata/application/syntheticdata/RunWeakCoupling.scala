package org.openmole.spatialdata.application.syntheticdata

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.synthetic.RandomGridGenerator
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.network.synthetic.RandomNetworkGenerator
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.Statistics
import org.openmole.spatialdata.vector.synthetic.DensityPointsGenerator

import scala.util.Random

object RunWeakCoupling //extends App
                    {

  //runNullModel


  def runNullModel(): Unit = {

    val seedrng = new Random
    val repets = 80
    //val repets = 1
    val gridSize = 50

    def run(occ: Double,nodes: Int,links: Int, seed: Int, mode: String): Array[Double] = {
      println(s"Run : ${occ} ; ${nodes} ; ${links} ; ${mode} ; ${seed}")
      implicit val rng: Random = new Random(seed)

      val randomGrid = RandomGridGenerator(gridSize,occ).generateGrid

      val randomNetwork = mode match {
        case "random" => RandomNetworkGenerator(nodes,links,true,false,false).generateNetwork
        case "density" => RandomNetworkGenerator(nlinks = links,points=DensityPointsGenerator(nodes,randomGrid,true).generatePoints.asPointSeq,planarize = true).generateNetwork
      }

      val nwmeasures = NetworkMeasures(randomNetwork,pathSample = 1.0)

      val res = Array(
        GridMorphology.moran(randomGrid),
        GridMorphology.distanceMean(randomGrid),
        Statistics.entropy(randomGrid),
        Statistics.slope(randomGrid)._1,
        Statistics.slope(randomGrid)._2,
        nwmeasures.shortestPaths.get.betweenness.values.sum / nwmeasures.shortestPaths.get.betweenness.size,
        nwmeasures.shortestPaths.get.meanPathLength,
        nwmeasures.shortestPaths.get.meanEfficiency,
        nwmeasures.shortestPaths.get.diameter,
        nwmeasures.summary.get.nodes,
        nwmeasures.summary.get.totalLength,
        nwmeasures.summary.get.weakComponents
      )
      println(s"${res(6)} ; ${res(7)} ; ${res(8)} ; ${res(11)}")
      res
    }



    val seeds = Seq.fill(repets){seedrng.nextInt(100000)}

    //val(occupied,nnodes,nlinks) = (Seq(0.25),Seq(20),Seq(40))
    val occupied = Seq(0.25,0.5,0.75)
    val nnodes = Seq(10,15,20)
    val nlinks = Seq(20,30,40)
    val modes = Seq("random","density")

    val params = (for {
      occ <- occupied
      nodes <- nnodes
      links <- nlinks
      mode <- modes
      seed <- seeds
    } yield (occ,nodes,links,seed,mode)).zipWithIndex.map{case (p,i) => (p._1,p._2,p._3,p._4,p._5,i)}

    val res: Array[Array[Any]] = (for {
      p <- params
      occ = p._1
      nodes = p._2
      links = p._3
      seed = p._4
      mode = p._5
      id = p._6
    } yield run(occ,nodes,links,seed,mode)++Array(occ,nodes,links,mode,seed,id)).toArray

    CSV.writeCSV(res,"data/coupled/nullmodel.csv",";",Array("moran","distanceMean","entropy","slope","rsquared",
      "meanBetweenness","meanPathLength","meanEfficiency","diameter","nodes","totalLength","components","occupation","nodes",
      "links","mode","seed","id"
    ))

  }

}
