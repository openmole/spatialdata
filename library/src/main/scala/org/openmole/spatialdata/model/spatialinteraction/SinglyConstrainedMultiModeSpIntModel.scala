package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{Matrix, SparseMatrix}
import org.openmole.spatialdata.vector.SpatialField

/**
 *  ! does not use sparse distanceWeights (see single mode)
 *
 * @param modesObservedFlows modesObservedFlows
 * @param modesDistances modesDistances
 * @param originValues originValues
 * @param destinationValues destinationValues
 * @param fittedParams fittedParams
 * @param costFunction costFunction
 * @param modesPredictedFlows modesPredictedFlows
 */
case class SinglyConstrainedMultiModeSpIntModel(
                                             modesObservedFlows: Array[Matrix],
                                             modesDistances: Array[Matrix],
                                             originValues: SpatialField[Double],
                                             destinationValues: SpatialField[Double],
                                             fittedParams: Array[Double],
                                             costFunction: (Double,Double)=> Double = {case (d,d0) => math.exp(-d / d0)},
                                             modesPredictedFlows: Array[Matrix] = Array.empty[Matrix]
                                           ) extends FittedSpIntModel {


  /**
    * Total observed flows are the sum of all modes
    * @return
    */
  override def observedFlows: Matrix = modesObservedFlows.reduce(Matrix.msum)

  /**
    * Several ways to compute an aggregate distance:
    *  - Average distance weighted by flows
    *  - raw average
    *  - min distance (deterministic mode choice)
    * @return
    */
  override def distances: Matrix = modesDistances.reduce(Matrix.msum).map(_ / modesDistances.length)

  override def distanceWeights: Matrix = modesDistances.zip(fittedParams).map{case (dmat,d0) => dmat.map(d => costFunction(d,d0))}.reduce(Matrix.msum).map(_ / modesDistances.length)

  override def predictedFlows: Matrix = modesPredictedFlows.reduce(Matrix.msum)

  /**
    * rq: the generic function does not make sense as it fits itself in the end?
    * @return
    */
  override def fit(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SpatialInteractionModel => FittedSpIntModel = {
    s: SpatialInteractionModel => s match {
      case m: SinglyConstrainedMultiModeSpIntModel =>
        SinglyConstrainedMultiModeSpIntModel.fitSinglyConstrainedMultiModeSpIntModel(m,
          m.fittedParams,
          SinglyConstrainedMultiModeSpIntModel.averageTripLength
        )
      case _ => throw new IllegalArgumentException("Can not fit other type of models")
    }
  }


}


object SinglyConstrainedMultiModeSpIntModel {


  /**
    * ! ad hoc constructor: assumes that the combines sp int models have the same O/D values, take the first
    * @param models models
    * @return
    */
  def apply(models: Array[SpatialInteractionModel]): SinglyConstrainedMultiModeSpIntModel =
    SinglyConstrainedMultiModeSpIntModel(
      modesObservedFlows = models.map(_.observedFlows),
      modesDistances = models.map(_.distances),
      originValues = models(0).originValues,
      destinationValues = models(0).destinationValues,
      fittedParams = Array.fill(models.length)(1.0)
    )



  /**
    * average trip length for each mode
    * @param model model
    * @param flowMatrices flowMatrices
    * @return
    */
  def averageTripLength(model: SinglyConstrainedMultiModeSpIntModel, flowMatrices: Array[Matrix]): Array[Double] = {
    val phitots = flowMatrices.map(_.sum)
    val phinorm = flowMatrices.zip(phitots).map{case (phi,phitot) => phi.map(_ / phitot)}
    val wd = phinorm.zip(model.modesDistances).map{case (phi,d) => phi*d}
    wd.map(_.sum)
  }

  /**
    *  ! run with sparse matrices for perf (after having benchmarked sparse mat impls)
    * @param model Model to fir
    * @param objectiveFunction statistic compared between the two models
    * @param originConstraint constraints at the origin?
    * @param convergenceThreshold convergenceThreshold
    * @return
    */
  def fitSinglyConstrainedMultiModeSpIntModel(model: SinglyConstrainedMultiModeSpIntModel,
                                              initialValues: Array[Double],
                                              objectiveFunction: (SinglyConstrainedMultiModeSpIntModel,Array[Matrix]) => Array[Double] = averageTripLength,
                                              originConstraint: Boolean = true,
                                              convergenceThreshold: Double = 0.01
                                    )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SinglyConstrainedMultiModeSpIntModel = {

    utils.log("Fitting multi mode singly constrained spatial interaction model")

    // ! force a SparseMatrix here
    val origin = utils.timerLog[Unit,Matrix](_ => SparseMatrix(model.originValues.values.flatten.toArray,row = false),(),"origin column matrix")
    val destination = utils.timerLog[Unit,Matrix](_ => SparseMatrix(model.destinationValues.values.flatten.toArray,row = false),(),"destination column matrix")
    println(s"origin column mat = $origin")

    val obsObjective = utils.timerLog[Unit,Array[Double]](_ => objectiveFunction(model,model.modesObservedFlows),(),"objective cost function")
    utils.log(s"observed stat = ${obsObjective.toSeq}")

    val initialFlows = singlyConstrainedMultiModeFlows(
        origin,
        destination,
        model.modesDistances.zip(initialValues).map{case (dmat,d0) => dmat.map(model.costFunction(_,d0))},
        originConstraint
    )

    val initialModel = model.copy(modesPredictedFlows=initialFlows, fittedParams = initialValues)

    // State is (model including cost function, current parameter value, epsilon)
    // epsilon is max of epsilon for each mode
    def iterateCostParam(state: (SinglyConstrainedMultiModeSpIntModel,Double)):  (SinglyConstrainedMultiModeSpIntModel,Double) = {
      val t = System.currentTimeMillis()
      val model = state._1
      val fitparameters = model.fittedParams
      utils.log(s"parameter = ${fitparameters.toSeq}")
      val currentCostMatrices = utils.timerLog[Unit,Array[Matrix]](_ => model.modesDistances.zip(fitparameters).map{case (dmat,d0) => dmat.map(model.costFunction(_,d0))},(),"current cost matrix")
      val predictedFlows = utils.timerLog[Unit,Array[Matrix]](_ => singlyConstrainedMultiModeFlows(origin,destination,currentCostMatrices,originConstraint),(),"singly constrained flows")

      val predObjectives = utils.timerLog[Unit,Array[Double]](_ => objectiveFunction(model,predictedFlows),(),"predicted cost function")
      utils.log(s"predicted stat = ${predObjectives.toSeq}")

      // ! with the form exp(-d/d0), inverse than exp(-beta d)
      val newfitparameters = fitparameters.zip(predObjectives).zip(obsObjective).map{case ((d0,cbar),c) => d0*c/cbar}
      val newmodel = model.copy(modesPredictedFlows = predictedFlows, fittedParams = newfitparameters)
      val errors = predObjectives.zip(obsObjective).map{case (cbar,c) => math.abs(c - cbar)/c}
      utils.log(s"fit singly constr multi mode: errors = ${errors.toSeq} ; iteration in ${System.currentTimeMillis()-t}")
      (newmodel,errors.max(Ordering.Double.TotalOrdering))
    }

    val res = Iterator.iterate((initialModel,Double.MaxValue.toDouble))(iterateCostParam).takeWhile(_._2>convergenceThreshold).toSeq.last
    res._1
  }



  /**
    * Multi mode flows (normalization is done on sum of all modes)
    *
    * Do a separate function than the single mode for readibility of usage in single mode
    *
    * @param originMasses originMasses
    * @param destinationMasses destinationMasses
    * @param costMatrices costMatrix
    * @param originConstraint originConstraint
    * @return
    */
  def singlyConstrainedMultiModeFlows(originMasses: Matrix,
                                      destinationMasses: Matrix,
                                      costMatrices: Array[Matrix],
                                      originConstraint: Boolean
                                     )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): Array[Matrix] = {
    val totalCostMatrix = costMatrices.reduce(Matrix.msum) // cost Theta(c * NModes)
    val normalization = (if (originConstraint) SparseMatrix.diagonal((totalCostMatrix %*% destinationMasses).values.flatten) else SparseMatrix.diagonal((totalCostMatrix %*% originMasses).values.flatten)).map(1 / _)

    val origin = SparseMatrix.diagonal(originMasses.flatValues)
    val destination = SparseMatrix.diagonal(destinationMasses.flatValues)
    val omat = if (originConstraint) origin %*% normalization else origin
    val dmat = if (originConstraint) destination else destination %*% normalization
    costMatrices.map(omat %*% _ %*% dmat) // cost Theta(c * NModes)
  }

}




