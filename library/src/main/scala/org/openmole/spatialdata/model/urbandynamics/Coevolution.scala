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
    val p = populationMatrix.ncols
    val res: RealMatrix = RealMatrix.zeros(n, p)
    res.setSubmatM(0, 0, populationMatrix.getSubmat(0, 0, nrows = n, ncols= 1).values)

    val gravityDistanceWeights = distancesMatrices(0).map{ d => Math.exp(-d / gravityDecay) }.asInstanceOf[RealMatrix]

    val feedbackDistanceWeights: Matrix = if(feedbackWeight!=0.0) feedbackDistancesMatrix.map { d => Math.exp(-d / feedbackDecay) } else EmptyMatrix()

    utils.log("mean dist mat : " + distancesMatrices(0).mean)
    utils.log("mean feedback mat : " + feedbackDistancesMatrix.mean)

    for (t <- 1 until p) {
      // get time between two dates
      val delta_t = dates(t) - dates(t - 1)

      val prevpop = res.getSubmat(0, t - 1, nrows = n, ncols = 1)
      val totalpop = prevpop.sum
      val diag: RealMatrix = RealMatrix.zeros(n,n);diag.setDiagM(prevpop.flatValues)
      val diagpops: RealMatrix = (diag * (1.0 / totalpop)).map(Math.pow(_, gravityGamma)).asInstanceOf[RealMatrix]

      utils.log(s"mean norm pop before time ${dates(t)} = " + diagpops.mean)

      val potsgravity: RealMatrix = (diagpops%*%gravityDistanceWeights%*%diagpops).asInstanceOf[RealMatrix]
      potsgravity.setDiagM(0.0)
      val meanpotgravity = potsgravity.sum / (n * n)

      val diagpopsFeedback: Matrix = (if(feedbackWeight!= 0.0) diagpops %*% DenseMatrix.ones(n,n) %*% diagpops else EmptyMatrix()).map(Math.pow(_, feedbackGamma))
      val potsfeedback: RealMatrix = (feedbackDistanceWeights %*% flattenPot(diagpopsFeedback)).asInstanceOf[RealMatrix]
      potsfeedback.setDiagM(0.0)
      val meanpotfeedback = potsfeedback.sum / n

      utils.log("mean pot gravity : " + meanpotgravity)
      utils.log("mean pot feedback : " + meanpotfeedback)

      val growthRates: Matrix = if (feedbackWeight != 0.0)
        RealMatrix.constant(n,1,growthRate) + ((potsgravity %*% RealMatrix.ones(n,1)) * (gravityWeight / (n * meanpotgravity))) + (potsfeedback * (2 * feedbackWeight / (n * (n - 1) * meanpotfeedback)))
      else RealMatrix.constant(n,1,growthRate) + (potsgravity %*% RealMatrix.ones(n,1)) * (gravityWeight / (n * meanpotgravity))
      // rq: not optimal to wrap/unwrap
      res.setSubmatM(0, t,
        (prevpop + (prevpop *
            (growthRates * delta_t)
          )
        ).values
      )
    }

    MacroResult(populationMatrix,res)
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

  /**
    * Transforms feedback potential into a flat vector
    * @param m potential matrix
    * @return
    */
  def flattenPot(m: Matrix): Matrix = {
    val n = m.nrows
    val res = DenseMatrix.zeros(n * (n - 1) / 2, 1)

    for (i <- 0 to n - 2) {
      //println("i :" + i)
      //println("range : " + ((i * (n - 1)) - (i * (i - 1) / 2)) + " ; " + ((i + 1) * (n - 1) - (i * (i + 1) / 2)))
      // rows \in i+1,n-1 ; cols in i,i
      val col = m.getSubmat(i + 1, i, nrows = n - i - 1, ncols = 1)
      //println(col.getRowDimension() + " ; " + col.getColumnDimension())
      //println((i + 1) * (n - 1) - (i * (i + 1) / 2) - (i * (n - 1)) - (i * (i - 1) / 2))
      // set row: (i * (n - 1)) - (i * (i - 1) / 2) , (i + 1) * (n - 1) - (i * (i + 1) / 2) - 1 ; set col: 0 (flat)
      res.setMSubmat((i * (n - 1)) - (i * (i - 1) / 2), 0, col.values)
    }
    res
  }



}


