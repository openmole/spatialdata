package org.openmole.spatialdata.application.multiscalemicro

import org.openmole.spatialdata.network.synthetic.GridNetworkGenerator
import org.openmole.spatialdata.network.{Link, Network, Node}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Convolution
import org.openmole.spatialdata.utils.visualization
import org.openmole.spatialdata.vector.{Attributes, Polygons}
import org.openmole.spatialdata.vector.synthetic.RandomPointsGenerator
import org.locationtech.jts.geom

import scala.collection.mutable
import scala.util.Random


/**
  *
  * @param worldSize world size in number of patches
  * @param patchSize patch size in meters
  * @param nCenters number of initial centers
  * @param mesoStepInterval iterations between evolutions of the network
  * @param steps total number of meso steps
  * @param seed seed
  * @param transportationLinkSpeed relative speed of infrastructure links
  */
case class MultiscaleMicro(
                          worldSize: Int,
                          patchSize: Double = 1000, // 1km by default
                          nCenters: Int,
                          mesoStepInterval: Int = 10,
                          steps: Int,
                          seed: Long,
                          transportationLinkSpeed: Double,
                          developersNumber: Int,
                          developerSetupMode: String,
                          lambdaDensity: Double = 0.001 // ~ 1 cell by default -> cut-off average kernel at 5 cells
                          ) {

  def run(): MultiscaleMicro.Result = MultiscaleMicro.run(this)

}



object MultiscaleMicro {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  val densityKernelCutOff: Double = 0.01

  // fixed initial config parameters
  val initialBuildingsPerCenter: Int = 50
  val initialBuildingsWidth: Double = 20.0
  val initialBuildingsHeight: Int = 4
  val initialShareHousing: Double = 0.5

  val streetBuffer: Double = 5.0 // dense urban areas

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

  object State {



    /**
      * Initial state
      * @param worldSize meso raster size
      * @param patchSize patch real world size
      * @param nCenters number of centers
      * @param transportationLinkSpeed transport link speed
      * @param rng rng
      * @return
      */
    def initialState(worldSize: Int, patchSize: Double, nCenters: Int, transportationLinkSpeed: Double, nDevelopers: Int, developerSetupMode: String, lambdaDensity: Double)(implicit rng: Random): State = {
      val nw = TransportationNetwork.initialNetwork(worldSize, patchSize, nCenters, transportationLinkSpeed)
      //val buildings  = State.initialBuildings()
      val grid = Grid.initialGrid(nw, patchSize, lambdaDensity)
      val stateWithoutInitialBuildings = State(grid, nw, Polygons.empty, DeveloperAgent.initialDeveloperAgents(nDevelopers, developerSetupMode))
      val centers = stateWithoutInitialBuildings.network.centres
      def iterateCenters(s: (State, Seq[Node])): (State, Seq[Node]) = {
        val addedBuildings = addBuildings(s._1, initialBuildingsPerCenter, initialBuildingsWidth, initialBuildingsHeight, initialShareHousing, streetBuffer, _ => initialBuildingsPositioning(s._2.head), patchSize=patchSize, lambdaDensity = lambdaDensity)
        if (s._2.isEmpty) s
        else (addedBuildings, s._2.tail)
      }
      //Iterator.iterate((stateWithoutInitialBuildings,centers))(iterateCenters).takeWhile(_._2.nonEmpty).toSeq.last._1
      Iterator.iterate((stateWithoutInitialBuildings,centers))(iterateCenters).take(centers.size+1).toSeq.last._1
    }

    def initialBuildingsPositioning(center: Node): (Double, Double) = center.position


    /**
      * Add simple square single-use buildings to a State given a positioning function
      * @param currentState current state
      * @param numberToAdd number of buildings to add
      * @param buildingWidth width
      * @param buildingHeight height
      * @param shareHousing share of housing
      * @param positioning positioning function
      * @return
      */
    def addBuildings(currentState: State, numberToAdd: Int, buildingWidth: Double, buildingHeight: Int, shareHousing: Double, streetBuffer: Double, positioning: State => (Double, Double), patchSize: Double, lambdaDensity: Double)(implicit rng: Random): State = {

      val (xmin,xmax,ymin,ymax) = bbox(currentState)
      val (x, y) = positioning(currentState)
      val (xk,yk) = ( ((x - xmin) / (buildingWidth + streetBuffer)).toInt, ((y - ymin) / (buildingWidth + streetBuffer)).toInt)

      utils.log(s"Adding $numberToAdd buildings around position ($x,$y) in [$xmin,$xmax]x[$ymin,$ymax]")

      //val kxs = 0 to (xmax - xmin / (buildingWidth + streetBuffer)).toInt   //((- math.floor((x - xmin)/(buildingWidth + streetBuffer))).toInt until 0)++(0 to math.floor((xmax - x)/(buildingWidth + streetBuffer)).toInt)
      // cannot reasonably stage all possible positions - in a 10km world this yields (10000/15)^2 ~ 0.5 Mio
      val kxs = xk - 2*(patchSize / (buildingWidth + streetBuffer)).toInt to xk + 2*(patchSize / (buildingWidth + streetBuffer)).toInt
      //val kys = 0 to (xmax - xmin / (buildingWidth + streetBuffer)).toInt   // ((- math.floor((y - ymin)/(buildingWidth + streetBuffer))).toInt until 0)++(0 to math.floor((ymax - y)/(buildingWidth + streetBuffer)).toInt)
      val kys = yk - 2*(patchSize / (buildingWidth + streetBuffer)).toInt to yk + 2*(patchSize / (buildingWidth + streetBuffer)).toInt

      val xcoords = kxs.map(k => xmin + k.toDouble*(buildingWidth + streetBuffer))
      val ycoords = kys.map(k => ymin + k.toDouble*(buildingWidth + streetBuffer))
      //val (kxmin,kxmax,kymin,kymax) = (kxs.min, kxs.max, kys.min, kys.max)
      val ks: Seq[(Int, Int)] = kxs.flatMap(kx => kys.map(ky => (kx,ky)))
      val kdist: Seq[Double] = ks.map{case (kx,ky) => math.abs(kx-xk)+math.abs(ky-yk)}
      utils.log(s"All slots: ${kdist.size}")
      val coords: Seq[(Double,Double)] = xcoords.flatMap(x => ycoords.map(y => (x,y)))
      val counts: mutable.HashMap[(Int,Int),Int] = new mutable.HashMap[(Int,Int),Int] // map (kx,ky) => building count
      ks.foreach(counts(_)=0)
      val existingBuildings = currentState.buildings
      utils.log(s"Previous buildings: ${existingBuildings.polygons.size}")
      existingBuildings.polygons.foreach{p =>
        val e = p.getEnvelopeInternal
        val (pxmin,pxmax,pymin,pymax) = (e.getMinX, e.getMaxX, e.getMinY, e.getMaxY)
        val pkxs = math.floor((pxmin - xmin) / (buildingWidth + streetBuffer)).toInt to math.floor((pxmax - xmin) / (buildingWidth + streetBuffer)).toInt
        val pkys = math.floor((pymin - ymin) / (buildingWidth + streetBuffer)).toInt to math.floor((pymax - ymax) / (buildingWidth + streetBuffer)).toInt
        //println("pkxs = "+pkxs); println(pkys)
        pkxs.foreach(pkx => pkys.foreach(pky => counts((pkx,pky)) = counts((pkx,pky))+1))
      }
      val free = ks.map{counts(_)==0}
      utils.log(s"  -> free slots: ${free.count(b => b)} - to be sorted")
      val buildingCentroids = coords.zip(free.zip(kdist)).filter(_._2._1).sortBy(_._2._2).take(numberToAdd).map(_._1)
      utils.log(s"Building centroids: $buildingCentroids")
      val buildingsToAdd = buildingsFromCentroids(buildingCentroids, buildingWidth, buildingHeight, shareHousing)
      val buildings = existingBuildings++buildingsToAdd

      // network and developers unchanged
      currentState.copy(
        grid = Grid.aggregateBuildingsToGrid(currentState.grid, buildings, lambdaDensity),
        buildings = buildings
      )
    }

    /**
      * Simple sqaure single-use buildings from centroids
      *  Attributes coding:
      *    - "type": 0 (housing) or 1 (office) (index in LU sequences)
      *    - "height": number of storeys
      *
      * @param centroids centroids
      * @param width building width
      * @param height building height
      * @param shareHousing share of housing (stochastic: function drawn randomly for each building)
      * @return
      */
    def buildingsFromCentroids(centroids: Seq[(Double,Double)], width: Double, height: Int, shareHousing: Double)(implicit rng: Random): Polygons = {
      val fact = new geom.GeometryFactory
      val polygons: Seq[geom.Polygon] = centroids.map{case (x,y) => fact.createPolygon(Array(new geom.Coordinate(x - width/2,y - width / 2),new geom.Coordinate(x - width/2, y + width / 2),new geom.Coordinate(x + width / 2, y + width / 2),new geom.Coordinate(x + width / 2, y - width / 2),new geom.Coordinate(x - width / 2,y - width / 2)))}
      val attrs: Seq[Attributes] = centroids.map(_ => {Map("type" -> (if (rng.nextDouble()<shareHousing) 0 else 1).asInstanceOf[AnyRef], "height" -> height.asInstanceOf[AnyRef])})
      Polygons(polygons, attrs)
    }

    def bbox(state: State): (Double, Double, Double, Double) = {
      val nodes = state.network.network.nodes
      val (x,y) = (nodes.map(_.position._1),nodes.map(_.position._2))
      val buffer = state.grid.patchSize
      (x.min - buffer, x.max + buffer, y.min - buffer, y.max + buffer)
    }


  }

  /**
    * Grid areas at the meso scale; not needed to represent it as 2D (network has coordinates), use indices map to associate node id to patch
    *  Population and employments are aggregated from buildings and used to evolve the network
    *
    * Note: add accessibilities as explicative variables?
    *
    * @param patchCoordinates coordinates of patch centers
    * @param patchDistancesToNetwork distance to closest station - not differentiated by h/w (not access)
    * @param patchDistancesToCentre distance to closest centre - idem
    * @param patchPopulations populations
    * @param patchEmployments employments
    * @param patchPopulationDensities population densities
    * @param patchEmploymentDensities employment densities
    * @param nodesIndices matching node id -> patch index in sequences
    * @param patch2DIndices 2D -> 1D
    * @param patchIndices 1D -> 2D
    * @param patchSize patch size in meters
    */
  case class Grid(
                 patchCoordinates: Seq[(Double, Double)],
                 patchDistancesToNetwork: Seq[Double],
                 patchDistancesToCentre: Seq[Double],
                 patchPopulations: Seq[Double],
                 patchEmployments: Seq[Double],
                 patchPopulationDensities: Seq[Double],
                 patchEmploymentDensities: Seq[Double],
                 nodesIndices: Map[Int,Int],
                 patch2DIndices: Map[(Int,Int),Int],
                 patchIndices: Map[Int,(Int,Int)],
                 patchSize: Double
                 )

  object Grid {

    /**
      * Initial grid given a network and initial buildings
      *
      * @param initialNetwork initial network
      * @return
      */
    def initialGrid(initialNetwork: TransportationNetwork, patchSize: Double, lambdaDensity: Double): Grid = {
      val nodesidseq = initialNetwork.network.nodes.map(_.id).toSeq
      // each node is the center of a patch
      val (distToStationsMap, distToCentresMap) = distances(initialNetwork)
      val coordsMap = initialNetwork.network.nodes.map(n => (n.id,n.position)).toMap
      val empty = emptyGrid(nodesidseq, coordsMap, distToStationsMap, distToCentresMap, patchSize)
      aggregateBuildingsToGrid(empty, Polygons.empty, lambdaDensity)
    }

    def emptyGrid(nodesidseq: Seq[Int], coordsMap: Map[Int, (Double,Double)], distToStationsMap: Map[Int, Double], distToCentresMap: Map[Int, Double], patchSize: Double): Grid = {
      val indices: Map[Int,Int] = nodesidseq.zipWithIndex.toMap
      val (xmin,ymin) = (coordsMap.values.map(_._1).min, coordsMap.values.map(_._2).min)
      val patchIndices = coordsMap.map{case (k,(x,y)) => (k, (math.floor((x - xmin) / patchSize).toInt, math.floor((y - ymin) / patchSize).toInt))}
      val patch2DIndices = patchIndices.map(_.swap)
      Grid(
        patchCoordinates = nodesidseq.map(coordsMap(_)),
        patchDistancesToNetwork = nodesidseq.map(distToStationsMap(_)),
        patchDistancesToCentre = nodesidseq.map(distToCentresMap(_)),
        patchPopulations = Seq.fill(nodesidseq.size)(0.0),
        patchEmployments = Seq.fill(nodesidseq.size)(0.0),
        patchPopulationDensities = Seq.fill(nodesidseq.size)(0.0),
        patchEmploymentDensities = Seq.fill(nodesidseq.size)(0.0),
        nodesIndices = indices,
        patch2DIndices = patch2DIndices,
        patchIndices = patchIndices,
        patchSize = patchSize
      )
    }

    /**
      * compute distance from nodes to stations and centres
      *  indices are not needed as nw nodes are fixed
      * @param nw network
      * @return maps Node id => distance for stations and centres
      */
    def distances(nw: TransportationNetwork): (Map[Int,Double],Map[Int,Double]) = {
      val nodesseq = nw.network.nodes.toSeq
      val distToStationsMap: Map[Int,Double] = nw.network.shortestPathTo(nodesseq,nw.stations).toSeq.groupMap(_._1._1)(_._2._3).map{case (n,dists) => (n.id, dists.min)}
      val distToCentresMap: Map[Int,Double] = nw.network.shortestPathTo(nodesseq,nw.centres).toSeq.groupMap(_._1._1)(_._2._3).map{case (n,dists) => (n.id, dists.min)}
      (distToStationsMap, distToCentresMap)
    }

    /**
      * recompute counts and densities in grid cells given buildings
      *  Note: having mutable hashmap to cumulate buildings and counts would be more performant?
      *
      *  Aggregate just at building centroids, assume no buildings spanning multiple patches (values for patchSize and building width must be scaled accordingly)
      *
      * @param previousGrid previous grid
      * @param buildings buildings
      * @return
      */
    def aggregateBuildingsToGrid(previousGrid: Grid, buildings: Polygons, lambdaDensity: Double): Grid = {
      if (buildings.polygons.isEmpty) previousGrid else {
        val popCounts: mutable.HashMap[(Int, Int), Double] = new mutable.HashMap[(Int, Int), Double]
        val jobCounts: mutable.HashMap[(Int, Int), Double] = new mutable.HashMap[(Int, Int), Double]
        previousGrid.patch2DIndices.keys.foreach{k => popCounts(k)=0.0; jobCounts(k)=0.0}

        val patchSize = previousGrid.patchSize
        val (xmin, ymin) = (previousGrid.patchCoordinates.map(_._1).min, previousGrid.patchCoordinates.map(_._2).min)
        buildings.foreach { case (p, a) =>
          val pc = p.getCentroid
          val pe = p.getEnvelopeInternal
          val (px, py) = (pc.getX, pc.getY)
          val surface = (pe.getMaxX - pe.getMinX) * (pe.getMaxY - pe.getMinY)
          val storeys = a.getOrElse("height", 1).asInstanceOf[Int]
          val key = (math.floor((px - xmin) / patchSize).toInt, math.floor((py - ymin) / patchSize).toInt)
          if (popCounts.contains(key)&&jobCounts.contains(key)) { // buildings not counted if outside of the world
            if (a.getOrElse("type", 0).asInstanceOf[Int] == 0) popCounts(key) = popCounts(key) + (surface * storeys) else jobCounts(key) = jobCounts(key) + (surface * storeys)
          }
        }
        // compute spatial average for densities: map as arrays
        val popRaster: Array[Array[Double]] = Convolution.mapAs2DArray(popCounts.toMap)
        val jobRaster: Array[Array[Double]] = Convolution.mapAs2DArray(jobCounts.toMap)
        // averaging kernel: distance to border cell is k*patchSize with a kernel of width 2*k+1
        // with an exponential kernel exp(-lambda d), cutting at 1% gives k = ceil(-ln(0.01)/(lambda*patchSize)) = ceil(4.6) for lambda = 0.001
        val k = math.ceil(-math.log(densityKernelCutOff) / (lambdaDensity * patchSize)).toInt
        val avgKernelUnnorm = (-k to k).toArray.map(kx => (-k to k).toArray.map(ky => math.exp(-lambdaDensity * patchSize * math.sqrt(kx * kx + ky * ky))))
        val s = avgKernelUnnorm.flatten.sum
        val avgKernel: Array[Array[Double]] = avgKernelUnnorm.map(_.map(_ / s))
        val avgPopDensities: Array[Array[Double]] = Convolution.convolution2D(popRaster, avgKernel)
        val avgJobDensities: Array[Array[Double]] = Convolution.convolution2D(jobRaster, avgKernel)
        val populations: Seq[Double] = Seq.tabulate(previousGrid.patchCoordinates.size)(i => popCounts(previousGrid.patchIndices(i)))
        val jobs: Seq[Double] = Seq.tabulate(previousGrid.patchCoordinates.size)(i => popCounts(previousGrid.patchIndices(i)))
        val popDensities: Seq[Double] = Seq.tabulate(previousGrid.patchCoordinates.size) { i => val (kx, ky) = previousGrid.patchIndices(i); avgPopDensities(kx)(ky) }
        val jobDensities: Seq[Double] = Seq.tabulate(previousGrid.patchCoordinates.size) { i => val (kx, ky) = previousGrid.patchIndices(i); avgJobDensities(kx)(ky) }
        previousGrid.copy(
          patchPopulations = populations,
          patchEmployments = jobs,
          patchPopulationDensities = popDensities,
          patchEmploymentDensities = jobDensities
        )
      }
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
      utils.log(s"Network nodes coordinates in [${nw.nodes.map(_.position._1).min}, ${nw.nodes.map(_.position._1).max}] x [${nw.nodes.map(_.position._2).min}, ${nw.nodes.map(_.position._2).max}]")
      TransportationNetwork(nw, centres, initialTrLinks.toSeq, centres)
    }
  }


  /**
    *
    * rq: uniform agents is less interesting? still random choice, should have more various shapes than deterministic case
    *
    * @param utilityWeights for each land-use, weight for each variable // //utilityDensityWeights: Seq[Double]
    * @param projectFloorSpace floor space per time step
    */
  case class DeveloperAgent(
                             utilityWeights: Seq[Seq[Double]],
                             projectFloorSpace: Double
                           )

  object DeveloperAgent {
    def initialDeveloperAgents(nAgents: Int, mode: String): Seq[DeveloperAgent] = {
      mode match {
        case "uniform" => Seq.fill(nAgents)(DeveloperAgent(Seq.empty,0.0))
        case _ => Seq.empty[DeveloperAgent]
      }
    }

    // not needed, can match on Strings, used only here
    //sealed trait DeveloperAgentsSetupMode
    //case object DeveloperAgentsUniformSetup extends DeveloperAgentsSetupMode
    //case object DeveloperAgentsHeterogenousSetup extends DeveloperAgentsSetupMode
  }

  case class Result(
                   states: Seq[State]
                   )



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

    val s0 = State.initialState(
      worldSize = worldSize,
      patchSize = patchSize,
      nCenters = nCenters,
      transportationLinkSpeed = transportationLinkSpeed,
      nDevelopers = developersNumber,
      developerSetupMode = developerSetupMode,
      lambdaDensity=lambdaDensity
    )

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

    val hmax = lastpolys.attributes.map(_.getOrElse("height",0).asInstanceOf[Int]).max
    val (xcoords,ycoords) = nws.flatMap(_.nodes.toSeq.map(_.position)).unzip
    val (minx,maxx,miny,maxy) = (xcoords.min,xcoords.max,ycoords.min,ycoords.max)
    val polygons = Seq(lastpolys.copy(
      attributes = lastpolys.attributes.map(a => (a.toSeq++Seq("color" -> a.getOrElse("height",0).asInstanceOf[Int].toDouble/hmax.toDouble)).toMap.asInstanceOf[Attributes]),
      polygons = lastpolys.rescale((minx,maxx,miny,maxy)).polygons
    ))
    visualization.staticVectorVisualization(
      networks =nws,
      edgeScaling = {l => if (lasttrnw.links.contains(l)) 5.0 else 0.0},
      edgeColoring = {l => if (lasttrnw.links.contains(l)) 2 else 0},
      nodeColoring = {n => if(lasttrnw.centres.contains(n)) 2 else 1 },
      nodePositioning = visualization.normalizedPositionNode(nws),
      nodeScaling =  {n => if(lasttrnw.centres.contains(n)) 5.0 else 0.0 },
      nodeShaping = {_ => 1},
      polygons = polygons,
      polygonsScaleColoringAttributes = Seq("color")
    )
  }

}