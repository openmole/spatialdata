package org.openmole.spatialdata.application.multiscalemicro

import org.openmole.spatialdata.grid.RasterLayerData
import org.openmole.spatialdata.network.synthetic.GridNetworkGenerator
import org.openmole.spatialdata.network.{Link, Network, Node}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Stochastic
import org.openmole.spatialdata.utils.visualization
import org.openmole.spatialdata.vector.Polygons
import org.openmole.spatialdata.vector.synthetic.RandomPointsGenerator

import scala.util.Random


case class MultiscaleMicro(
                          worldSize: Int,
                          patchSize: Double,
                          nCenters: Int,
                          mesoStepInterval: Int,
                          steps: Int,
                          seed: Long,
                          transportationLinkSpeed: Double
                          ) {

  def run(): MultiscaleMicro.Result = MultiscaleMicro.run(this)

}



object MultiscaleMicro {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  /**
    * State of the model
    *  Regional agent not included (only one), but developers are (may be changed)
    *
    * @param grid patches (meso)
    * @param network transportation network (meso)
    * @param buildings building polygons (micro)
    */
  case class State(
                  grid: Grid,
                  network: TransportationNetwork,
                  buildings: Polygons,
                  developers: Seq[DeveloperAgent]
                  )

  /**
    * Grid areas at the meso scale; not needed to represent it as 2D (network has coordinates), use indices map to associate node id to patch
    *  Population and employments are aggregated from buildings and used to evolve the network
    *
    * @param patchDistancesToNetwork distance to closest station
    * @param patchDistancesToCentre distance to closest centre
    * @param patchPopulations populations
    * @param pathEmployments employments
    * @param indices matching node id -> patch index in sequences
    */
  case class Grid(
                 patchDistancesToNetwork: Seq[Double],
                 patchDistancesToCentre: Seq[Double],
                 patchPopulations: Seq[Double],
                 pathEmployments: Seq[Double],
                 indices: Map[Int,Int]
                 )

  object Grid {
    /**
      * Empty grid given a network
      * @param initialNetwork initial network
      * @return
      */
    def initialGrid(initialNetwork: TransportationNetwork): Grid = {
      val nodesidseq = initialNetwork.network.nodes.map(_.id).toSeq
      val indices: Map[Int,Int] = nodesidseq.zipWithIndex.toMap
      // each node is the center of a patch
      val (distToStationsMap, distToCentresMap) = distances(initialNetwork)
      Grid(nodesidseq.map(distToStationsMap(_)),nodesidseq.map(distToCentresMap(_)),Seq.fill(nodesidseq.size)(0.0),Seq.fill(nodesidseq.size)(0.0),indices)
    }

    /**
      * compute distance from nodes to stations and centres
      *  indices are not needed as nw nodes are fixed
      * @param nw network
      * @return
      */
    def distances(nw: TransportationNetwork): (Map[Int,Double],Map[Int,Double]) = {
      val nodesseq = nw.network.nodes.toSeq
      val distToStationsMap: Map[Int,Double] = nw.network.shortestPathTo(nodesseq,nw.stations).toSeq.groupMap(_._1._1)(_._2._3).map{case (n,dists) => (n.id, dists.min)}
      val distToCentresMap: Map[Int,Double] = nw.network.shortestPathTo(nodesseq,nw.centres).toSeq.groupMap(_._1._1)(_._2._3).map{case (n,dists) => (n.id, dists.min)}
      (distToStationsMap, distToCentresMap)
    }

  }

  /**
    * Transportation network, overlaying diffuse road network (grid network linking neighbour patches)
    *   with a higher speed infrastructure network (public transport for example)
    * @param network network
    * @param stations infrastructure network stations
    * @param links infrastructure links
    * @param centres amenity centres (included here to simplify state representation)
    */
  case class TransportationNetwork(
                                  network: Network,
                                  stations: Seq[Node],
                                  links: Seq[Link],
                                  centres: Seq[Node]
                                  )

  object TransportationNetwork {

    /**
      * initial grid network with random centers
      *  initial stations are centers, connected
      * @param worldSize world size
      * @param patchSize path size
      * @param nCenters number of centers
      * @param rng random
      * @return
      */
    def initialNetwork(worldSize: Int, patchSize: Double, nCenters: Int, transportationLinkSpeed: Double)(implicit rng: Random): TransportationNetwork = {
      val gridnw = GridNetworkGenerator((worldSize*patchSize).toInt, patchSize, patchSize, withDiagLinks = true).generateNetwork
      utils.log("Grid nw: |V|="+gridnw.nodes.size+" ; |E|="+gridnw.links.size)
      val centreCoords = RandomPointsGenerator(nCenters).generatePoints.asPointSeq.map{case (x,y) => (x*worldSize*patchSize,y*worldSize*patchSize)}
      val (xc,yc) = (centreCoords.map(_._1).sum / nCenters.toDouble,centreCoords.map(_._2).sum / nCenters.toDouble)
      // (xc',yc') = worldSize*patchSize / 2 => Delta x = worldSize*patchSize / 2 - xc => x' = x + worldSize*patchSize / 2 - xc
      val translatedCoords = centreCoords.map{case (x,y) => (x + (worldSize*patchSize) / 2 - xc, y + (worldSize*patchSize) / 2 - yc)}
      // find closest nodes
      val centres: Seq[Node] = translatedCoords.map {case (x,y) =>
        gridnw.nodes.map(n => (math.sqrt(math.pow(n.position._1 - x,2.0) + math.pow(n.position._2 - y,2.0)),n)).minBy(_._1)._2
      }
      utils.log("Centres: "+centres)
      // connect initial stations
      val initialTrLinks = Network(centres.toSet, Set.empty[Link]).weakComponentConnect.links.map(_.copy(weight = 1 / transportationLinkSpeed))
      val nw = gridnw.addLinks(initialTrLinks)
      TransportationNetwork(nw, centres, initialTrLinks.toSeq, centres)
    }
  }

  case class DeveloperAgent(

                           )

  object DeveloperAgent {
    def initialDeveloperAgents(): Seq[DeveloperAgent] = Seq.empty[DeveloperAgent]
  }

  case class Result(
                   states: Seq[State]
                   )


  /**
    * Initial state
    * @param worldSize meso raster size
    * @param patchSize patch real world size
    * @param nCenters number of centers
    * @param transportationLinkSpeed transport link speed
    * @param rng rng
    * @return
    */
  def initialState(worldSize: Int, patchSize: Double, nCenters: Int, transportationLinkSpeed: Double)(implicit rng: Random): State = {
    val nw = TransportationNetwork.initialNetwork(worldSize, patchSize, nCenters, transportationLinkSpeed)
    val grid = Grid.initialGrid(nw)
    State(grid, nw, Polygons.empty, DeveloperAgent.initialDeveloperAgents())
  }

  /**
    * One model step
    * @param model model
    * @param state state
    * @return
    */
  def step(model: MultiscaleMicro, state: State): State = {
    state
  }

  /**
    * Run the model
    * @param model model
    * @return
    */
  def run(model: MultiscaleMicro): MultiscaleMicro.Result = {
    import model._

    implicit val rng: Random = new Random(seed)

    val s0 = initialState(worldSize, patchSize, nCenters, transportationLinkSpeed)

    MultiscaleMicro.Result(
      Seq(s0)++Iterator.iterate(s0){s: State => step(model,s)}.take(steps).toSeq
    )
  }

  /**
    * visualise result
    * @param result results
    */
  def visualize(result: Result): Unit = {
    // vis nw
    val lasttrnw = result.states.last.network
    val nws =  Seq(lasttrnw.network)
    val lastpolys = result.states.last.buildings
    val polygons = Seq(lastpolys)
    visualization.staticVectorVisualization(
      networks =nws,
      edgeScaling = {l => if (lasttrnw.links.contains(l)) 5.0 else 0.0},
      edgeColoring = {l => if (lasttrnw.links.contains(l)) 2 else 0},
      nodeColoring = {n => if(lasttrnw.centres.contains(n)) 2 else 1 },
      nodePositioning = visualization.normalizedPosition(nws),
      nodeScaling =  {n => if(lasttrnw.centres.contains(n)) 5.0 else 0.0 },
      nodeShaping = {_ => 1}
    )
  }

}