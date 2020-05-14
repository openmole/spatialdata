package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.utils.math._

/**
  *
  * Macroscopic coevolution model for systems of cities, described in
  *
  *   Raimbault, J. (2020). Indirect evidence of network effects in a system of cities. Environment and Planning B: Urban Analytics and City Science, 47(1), 138-155.
  *   Raimbault, J. (2018). Modeling the co-evolution of cities and networks. arXiv preprint arXiv:1804.09430.
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
  * @param m matrix implementation
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
                        implicit val m: MatrixImplementation
                      ) extends MacroModel {

  override def run: MacroResult = Coevolution.run(model = this)

  override def toString: String = "Coevolution model with parameters"+
    "\n\tgrowthRate = "+growthRate+"\n\tgravityWeight = "+gravityWeight+
    "\n\tgravityGamma = "+gravityGamma+"\n\tgravityDecay = "+gravityDecay+
    "\n\tfeedbackWeight = "+feedbackWeight+"\n\tfeedbackGamma = "+feedbackGamma+
    "\n\tfeedbackDecay = "+feedbackDecay

}



object Coevolution {

  // setup matrices
  /*def setup(populations: File, distances: File, feedbackDistances: File, datesFile: File) = {
    populationMatrix = FileUtils.parseMatrixFile(populations)
    distancesMatrix = FileUtils.parseMatrixFile(distances)
    feedbackDistancesMatrix = FileUtils.parseMatrixFile(feedbackDistances)
    dates = FileUtils.parseSimple(datesFile)
    //for (t <- 0 to feedbackDistancesMatrix.getColumnDimension() - 1) { print(feedbackDistancesMatrix.get(0, t) + " ; ") }
  }*/

  /**
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
            feedbackDecay: Double,
           )(implicit m: MatrixImplementation = Matrix.defaultImplementation): Coevolution = {

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

    val rawdates: Seq[String] = CSV.readCSV(datesFile,withHeader=false).values.toSeq(0)
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
      m
    )
  }


  /**
    * run a coevolution model
    * @param model model
    * @return
    */
  def run(model: Coevolution)(implicit mImpl: MatrixImplementation): MacroResult = {

    utils.log("Running "+model.toString)

    import model._

    val n = populationMatrix.nrows
    val gravityDistanceWeights = distancesMatrices(0).map{ d => Math.exp(-d / gravityDecay) }.asInstanceOf[RealMatrix]
    val feedbackDistanceWeights: Matrix = if(feedbackWeight!=0.0) feedbackDistancesMatrix.map { d => Math.exp(-d / feedbackDecay) } else EmptyMatrix()

    utils.log("mean dist mat : " + distancesMatrices(0).mean)
    utils.log("mean feedback mat : " + feedbackDistancesMatrix.mean)

    val growthRates = Vector(
      Gibrat(Vector.fill(n)(growthRate)),
      InteractionMacro(Vector.fill(n)(gravityDecay),Vector.fill(n)(gravityWeight),Vector.fill(n)(gravityGamma),gravityDistanceWeights)
    ) ++ (if (feedbackWeight != 0.0)
      Vector(NetworkFeedback(Vector.fill(n)(feedbackWeight),Vector.fill(n)(feedbackDecay),Vector.fill(n)(feedbackGamma),feedbackDistanceWeights)) else Vector.empty)

    def step(state: (Vector[MacroState],Vector[Double])): (Vector[MacroState],Vector[Double]) =
      (Vector(MacroModel.macroStep(state._1.head,growthRates,state._2.head))++state._1,state._2.tail)

    val initState = MacroState(0,populationMatrix.getSubmat(0, 0, nrows = n, ncols= 1), distancesMatrices(0))
    val deltats = dates.tail.zip(dates.dropRight(1)).map{case (next,prev)=> next-prev}.toVector

    val states = Iterator.iterate((Vector(initState),deltats))(step).takeWhile(_._2.nonEmpty).toVector.last._1

    MacroResult(populationMatrix,states)
  }


  /**
    * Direct flows between cities given a distance matrix
    */
  /*
  def computeFlows(populations: Populations, distances: Distances): Array[Array[Double]] = {
    Array.empty
  }*/

  /**
    *
    */
  /*
  def updatePopulations(populations: Populations,flows: Array[Array[Double]]): Populations = {
    Array.empty
  }*/

  /*
  def updateDistances(distances: Distances, flows: Array[Array[Double]]): Distances = {
    Array.empty
  }*/





}


