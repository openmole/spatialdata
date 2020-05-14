package org.openmole.spatialdata.model.urbandynamics
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix, RealMatrix}

case class NetworkFeedback(
                            feedbackWeights: Vector[Double],
                            feedbackDecays: Vector[Double],
                            feedbackGammas: Vector[Double],
                            feedbackDistanceWeights: Matrix
                          ) extends MacroGrowthRate {
  override def growthRate: Matrix => Matrix = m => NetworkFeedback.networkFeedbackGrowthRates(m,feedbackWeights,feedbackGammas,feedbackDistanceWeights)
}


object NetworkFeedback {

  def networkFeedbackGrowthRates(prevpop: Matrix,feedbackWeights: Vector[Double],feedbackGammas: Vector[Double], feedbackDistanceWeights: Matrix): Matrix = {
    implicit val mImpl: MatrixImplementation = Matrix.getImplementation(prevpop)
    val n = prevpop.nrows
    val totalpop = prevpop.sum
    val diagpops =  DenseMatrix.diagonal(prevpop.flatValues.zip(feedbackGammas).map{ case (p,g) => math.pow(p / totalpop,g)})
    val diagpopsFeedback: Matrix = diagpops %*% DenseMatrix.ones(n,n) %*% diagpops
    val potsfeedback: RealMatrix = (feedbackDistanceWeights %*% flattenPot(diagpopsFeedback)).asInstanceOf[RealMatrix]
    potsfeedback.setDiagM(0.0)
    val meanpotfeedback = potsfeedback.sum / n
    utils.log("mean pot feedback : " + meanpotfeedback)
    potsfeedback * Matrix(feedbackWeights.toArray,row=false) * (2 / (n * (n - 1) * meanpotfeedback))
  }


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
