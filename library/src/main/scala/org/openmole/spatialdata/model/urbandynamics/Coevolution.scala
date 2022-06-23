package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.model.urbandynamics.Coevolution.CoevolutionState
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.utils.math._

/**
  *
  * Macroscopic coevolution model for systems of cities, described in
  *
  *   Raimbault, J. (2020). Indirect evidence of network effects in a system of cities. Environment and Planning B: Urban Analytics and City Science, 47(1), 138-155.
  *   Raimbault, J. (2021). Modeling the co-evolution of cities and networks. In Handbook of Cities and Networks (pp. 166-193). Edward Elgar Publishing.
  *
  *   Physical network explored in
  *    Raimbault, J. (2020). Hierarchy and co-evolution processes in urban systems. arXiv preprint arXiv:2001.11989.
  *   is not yet implemented
  *
  * @param populationMatrix Target pop matrix; rows : cities ; cols : time
  * @param distancesMatrices Distance matrices (target in time)
  * @param feedbackDistancesMatrix Network feedback distance matrix
  * @param dates Dates
  * @param growthRate g_0
  * @param gravityWeight w_G
  * @param gravityGamma gamma_G
  * @param gravityDecay d_G
  * @param feedbackWeight w_N
  * @param feedbackGamma gamma_N
  * @param feedbackDecay d_N
  */
case class Coevolution(
                        populationMatrix: Matrix,
                        distancesMatrices: Array[Matrix],
                        feedbackDistancesMatrix: Matrix,
                        dates: Array[Double],
                        growthRate: Double,
                        gravityWeight: Double,
                        gravityGamma: Double,
                        gravityDecay: Double,
                        feedbackWeight: Double,
                        feedbackGamma: Double,
                        feedbackDecay: Double,
                        networkGmax: Double,
                        networkExponent: Double,
                        networkThresholdQuantile: Double,
                        //implicit val m: MatrixImplementation // find a proper way to specify matrix impl?
                      ) extends MacroModel {

  override def run: MacroResultFit = Coevolution.run(model = this)

  override def nextStep(state: MacroState, populations: Matrix, distanceMatrix: Matrix): MacroState = {
    val updatedState = Coevolution.updateState(state.asInstanceOf[CoevolutionState], populations, distanceMatrix)
    Coevolution.nextState(this, updatedState)
  }

  override def finalTime: Int = dates.length

  override def toString: String = "Coevolution model with parameters"+
    "\n\tgrowthRate = "+growthRate+"\n\tgravityWeight = "+gravityWeight+
    "\n\tgravityGamma = "+gravityGamma+"\n\tgravityDecay = "+gravityDecay+
    "\n\tfeedbackWeight = "+feedbackWeight+"\n\tfeedbackGamma = "+feedbackGamma+
    "\n\tfeedbackDecay = "+feedbackDecay

}



object Coevolution {


  case class CoevolutionState(
                             time: Int,
                             populations: Matrix,
                             distanceMatrix: Matrix,
                             interactionPotentials: Matrix,
                             flows: Matrix
                             ) extends MacroState {
    def asMacroStateGen: MacroStateGen = MacroStateGen(time, populations, distanceMatrix)
  }

  // setup matrices
  /*def setup(populations: File, distances: File, feedbackDistances: File, datesFile: File) = {
    populationMatrix = FileUtils.parseMatrixFile(populations)
    distancesMatrix = FileUtils.parseMatrixFile(distances)
    feedbackDistancesMatrix = FileUtils.parseMatrixFile(feedbackDistances)
    dates = FileUtils.parseSimple(datesFile)
    //for (t <- 0 to feedbackDistancesMatrix.getColumnDimension() - 1) { print(feedbackDistancesMatrix.get(0, t) + " ; ") }
  }*/

  /**
    * Construct from real data file and parameter values
    *  ! for fitting population only; fixed distance matrix
    *
    * @param populationsFile path for pop file
    * @param distancesFile path for distance matrix file
    * @param feedbackDistancesFile path for feedback distances file
    * @param datesFile path for date file
    * @param growthRate g_0
    * @param gravityWeight w_G
    * @param gravityGamma gamma_G
    * @param gravityDecay d_G
    * @param feedbackWeight w_N
    * @param feedbackGamma gamma_N
    * @param feedbackDecay d_N
    * @return
    */
  def apply(populationsFile: String,
            distancesFile : String,
            feedbackDistancesFile : String,
            datesFile: String,
            growthRate: Double,
            gravityWeight: Double,
            gravityGamma: Double,
            gravityDecay: Double,
            feedbackWeight: Double,
            feedbackGamma: Double,
            feedbackDecay: Double
           )(implicit m: MatrixImplementation = Matrix.defaultImplementation)
  : Coevolution = {

    val populationMatrix: Matrix = Matrix(CSV.readMat(populationsFile))
    val distancesMatrices: Array[Matrix] = distancesFile.length match {
      case n if n > 0 => Array(Matrix(CSV.readMat(distancesFile)))
      case _ => Array(DenseMatrix.zeros(populationMatrix.nrows,populationMatrix.ncols))
    }

    val feedbackDistancesMatrix: Matrix = feedbackWeight match {
      case 0.0 => EmptyMatrix()
      case _ => feedbackDistancesFile match {
        case null => DenseMatrix.zeros(populationMatrix.nrows,(populationMatrix.nrows*(populationMatrix.nrows-1))/2)
        case _ => Matrix(CSV.readMat(feedbackDistancesFile))
      }
    }

    val rawdates: Seq[String] = CSV.readCSV(datesFile,withHeader=false).values.toSeq.head
    val dates: Array[Double] = rawdates.map(_.toDouble).toArray

    Coevolution(
      populationMatrix,
      distancesMatrices,
      feedbackDistancesMatrix,
      dates,
      growthRate,
      gravityWeight,
      gravityGamma,
      gravityDecay,
      feedbackWeight,
      feedbackGamma,
      feedbackDecay,
      networkGmax = 0.0,
      networkExponent = 1.0,
      networkThresholdQuantile = 0.5
      //m = m
    )
  }

  def updateState(state: CoevolutionState, populations: Matrix, distanceMatrix: Matrix): CoevolutionState = {
    state.copy(populations = populations, distanceMatrix = distanceMatrix)
  }

  def initialState(model: Coevolution): CoevolutionState = {
    import model._
    val pops = populationMatrix.getCol(0)
    val dmat = model.distancesMatrices.head
    val gravityDistanceWeights = dmat.map{ d => Math.exp(-d / gravityDecay) }
    val totalpop = pops.sum
    val interactionPotentials = computeFlows(pops, gravityDistanceWeights, model.gravityGamma, {case (p,g) =>math.pow(p / totalpop,g) })
    val flows =  computeFlows(pops, gravityDistanceWeights, 1.0, {case (p,_) =>p / totalpop })

    CoevolutionState(0, pops, dmat, interactionPotentials, flows)
  }

  /**
   * One co-evolution step
   * @param model model
   * @param state state
   * @return
   */
  def nextState(model: Coevolution, state: CoevolutionState): CoevolutionState = {
    utils.log(s"\n----Coevol step ${state.time}")
    import model._
    val t = state.time

    val updatedPopulationState = MacroModel.macroStep(state.asMacroStateGen,growthRates(model),model.dates(t + 1) - model.dates(t))

    // should not be recomputed either - as flows
    val gravityDistanceWeights = updatedPopulationState.distanceMatrix.map{ d => Math.exp(-d / model.gravityDecay) }

    val totalpop = updatedPopulationState.populations.sum
    val interactionPotentials = computeFlows(updatedPopulationState.populations, gravityDistanceWeights, model.gravityGamma, {case (p,g) =>math.pow(p / totalpop,g) })

    val updatedNetwork = updateNetwork(updatedPopulationState.distanceMatrix, interactionPotentials, networkGmax, networkExponent, networkThresholdQuantile)

    val updatedGravityDistanceWeight = updatedNetwork.map{ d => Math.exp(-d / model.gravityDecay) }
    val flows = computeFlows(updatedPopulationState.populations, updatedGravityDistanceWeight, 1.0, {case (p,_) => p / totalpop})

    utils.log(s"Delta distance = ${updatedPopulationState.distanceMatrix.flatValues.zip(updatedNetwork.flatValues).map{case (d1,d2) =>math.abs(d1-d2)}.sum}")

    state.copy(populations = updatedPopulationState.populations, distanceMatrix = updatedNetwork, interactionPotentials = interactionPotentials, flows = flows, time = state.time + 1)
  }

  /**
    * Population growth rates
    * @param model model
    * @return
    */
  def growthRates(model: Coevolution): Vector[MacroGrowthRate] = {
    import model._
    val n = populationMatrix.nrows

    // note: recomputed at each time step? yes as distance evolve
    val gravityDistanceWeights = distancesMatrices(0).map{ d => Math.exp(-d / gravityDecay) }
    val feedbackDistanceWeights: Matrix = if(feedbackWeight!=0.0) feedbackDistancesMatrix.map { d => Math.exp(-d / feedbackDecay) } else EmptyMatrix()


    Vector(
      Gibrat(Vector.fill(n)(growthRate)),
      InteractionMacro(Vector.fill(n)(gravityDecay),Vector.fill(n)(gravityWeight),Vector.fill(n)(gravityGamma),gravityDistanceWeights)
    ) ++ (if (feedbackWeight != 0.0)
      Vector(NetworkFeedback(Vector.fill(n)(feedbackWeight),Vector.fill(n)(feedbackDecay),Vector.fill(n)(feedbackGamma),feedbackDistanceWeights)) else Vector.empty)
  }


  /**
    * run a coevolution model
    *  // (implicit mImpl: MatrixImplementation): not needed
    * @param model model
    * @return
    */
  def run(model: Coevolution): MacroResultFit = {

    utils.log("Running "+model.toString)

    import model._

    val n = populationMatrix.nrows
    //val gravityDistanceWeights = distancesMatrices(0).map{ d => Math.exp(-d / gravityDecay) }.asInstanceOf[RealMatrix]
    //val feedbackDistanceWeights: Matrix = if(feedbackWeight!=0.0) feedbackDistancesMatrix.map { d => Math.exp(-d / feedbackDecay) } else EmptyMatrix()

    utils.log("mean dist mat : " + distancesMatrices(0).mean)
    utils.log("mean feedback mat : " + feedbackDistancesMatrix.mean)

    def step(state: (Vector[MacroStateGen],Vector[Double])): (Vector[MacroStateGen],Vector[Double]) =
      (Vector(MacroModel.macroStep(state._1.head,growthRates(model),state._2.head))++state._1,state._2.tail)

    val initState = MacroStateGen(0,populationMatrix.getSubmat(0, 0, nrows = n, ncols= 1), distancesMatrices(0))
    val deltats = dates.tail.zip(dates.dropRight(1)).map{case (next,prev)=> next-prev}.toVector

    val finalState = Iterator.iterate((Vector(initState),deltats))(step).takeWhile(_._2.nonEmpty).toVector.last._1

    MacroResultFit(populationMatrix,finalState)
  }



  /**
   * Direct flows between cities given a distance matrix
    *  -> gravity flows used for growth rates
    *  ! shouldn't recompute - ok for now, very small matrices
   * @param populations populations
   * @param distances distances
   * @return
   */
  def computeFlows(populations: Matrix, distances: Matrix, interactionGamma: Double, rescaleFunction: (Double, Double) => Double): Matrix =
    InteractionMacro.gravityPotential(populations, distances, Vector.fill(populations.nrows)(interactionGamma),rescaleFunction)

  /**
   * Update virtual network (distance matrix)
    * d_ij (t+1) = d_ij (t) * (1 - g_max * \frac{ (phi_ij/phi_0) ^ \gamma - 1 }{(phi_ij/phi_0) ^ \gamma + 1}
    *
   * @param distances distances
   * @param flows flows
   * @return
   */
  def updateNetwork(distances: Matrix, flows: Matrix, gmax: Double, exponent: Double, thresholdQuantile: Double): Matrix = {
    val threshold = quantile(flows.flatValues, thresholdQuantile)
    distances * flows.map{phi => 1.0 - gmax*(math.pow(phi/threshold, exponent) - 1.0)/(math.pow(phi/threshold, exponent) + 1.0)}
  }





}


