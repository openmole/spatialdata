package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.model.spatialinteraction.SinglyConstrainedSpIntModel.averageTripLength
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{EmptyMatrix, Matrix, SparseMatrix}
import org.openmole.spatialdata.vector.SpatialField

/**
  *
  * @param observedFlows observed flows
  * @param distances distance matrix
  * @param costFunction The function on which parameter optimization is done
  * @param originValues origin
  * @param destinationValues destination
  * @param predictedFlows previous predicted flows
  */
case class SinglyConstrainedSpIntModel(
                                        observedFlows: Matrix,
                                        distances: Matrix,
                                        originValues: SpatialField[Double],
                                        destinationValues: SpatialField[Double],
                                        costFunction: (Double,Double)=> Double = {case (d,d0) => math.exp(-d / d0)},
                                        fittedParam: Double = 1.0,
                                        predictedFlows: Matrix = EmptyMatrix()
                                      ) extends FittedSpIntModel {

  /**
    * rq: the generic function does not make sense as it fits itself in the end?
    * @return
    */
  override def fit(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SpatialInteractionModel => FittedSpIntModel = {
    {
      case m: SinglyConstrainedSpIntModel => SinglyConstrainedSpIntModel.fitSinglyConstrainedSpIntModel(m, averageTripLength, 1.0, originConstraint = true, 0.01)
      case _ => throw new IllegalArgumentException("Can not fit other type of models")
    }
  }

  override def fittedParams: Array[Double] = Array(fittedParam)

}


object SinglyConstrainedSpIntModel {

  /**
    * copy from another model, e.g. synthetic or real unfitted flows
    * @param model model
    * @return
    */
  def apply(model: SpatialInteractionModel): SinglyConstrainedSpIntModel = SinglyConstrainedSpIntModel(model.observedFlows,model.distances,model.originValues,model.destinationValues)

  /**
    * one possible statistic to adjust the model
    * @param model model
    * @param phi phi
    * @return
    */
  def averageTripLength(model: SinglyConstrainedSpIntModel, phi: Matrix): Double = {
    //utils.log(s"Computing avg trip length for flows $phi and distmat ${model.distances}")
    val phitot = phi.sum
    //utils.log(s"phitot = $phitot")
    val phinorm = phi.map(_ / phitot)
    //utils.log("phinorm")
    val wd = phinorm*model.distances
    //utils.log("wd")
    wd.sum
  }

  /**
    *  ! run with sparse matrices for perf (after having benchmarked sparse mat impls)
    * @param model Model to fit
    * @param objectiveFunction statistic compared between the two models
    * @param originConstraint constraints at the origin?
    * @param convergenceThreshold convergence threshold
    * @return
    */
  def fitSinglyConstrainedSpIntModel(model: SinglyConstrainedSpIntModel,
                                     objectiveFunction: (SinglyConstrainedSpIntModel,Matrix) => Double = averageTripLength,
                                     initialValue: Double = 1.0,
                                     originConstraint: Boolean = true,
                                     convergenceThreshold: Double = 0.01
                                    )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SinglyConstrainedSpIntModel = {

    // ! force a SparseMatrix here
    val origin = utils.timerLog[Unit,Matrix](_ => SparseMatrix(model.originValues.values.flatten.toArray,row = false),(),"origin column matrix")
    val destination = utils.timerLog[Unit,Matrix](_ => SparseMatrix(model.destinationValues.values.flatten.toArray,row = false),(),"destination column matrix")
    println(s"origin column mat = ${origin}")

    val obsObjective = utils.timerLog[Unit,Double](_ => objectiveFunction(model,model.observedFlows),(),"objective cost function")
    utils.log(s"observed stat = $obsObjective")

    val initialFlows = utils.timerLog[Matrix](singlyConstrainedFlows(origin,destination,model.distances.map(model.costFunction(_,initialValue)),originConstraint),"initial flows")

    val initialModel = model.copy(predictedFlows=initialFlows, fittedParam = initialValue)

    /**
      * State is (model including cost function, current parameter value, epsilon)
      * @param state state
      * @return
      */
    def iterateCostParam(state: (SinglyConstrainedSpIntModel,Double)):  (SinglyConstrainedSpIntModel,Double) = {
      val t = System.currentTimeMillis()
      val model = state._1
      val fitparameter = model.fittedParam

      utils.log(s"parameter = $fitparameter")

      val currentCostMatrix = utils.timerLog[Unit,Matrix](_ => model.distances.map(model.costFunction(_,fitparameter)),(),"current cost matrix")
      //val currentCostMatrix = model.distances.map{d => math.exp(-d / fitparameter)}

      //utils.log(s"avg cost = ${currentCostMatrix.mean}")

      val predictedFlows = utils.timerLog[Unit,Matrix](_ => singlyConstrainedFlows(origin,destination,currentCostMatrix,originConstraint),(),"singly constrained flows")

      //utils.log(s"avg predicted flow = ${predictedFlows.mean}")

      val predObjective = utils.timerLog[Unit,Double](_ => objectiveFunction(model,predictedFlows),(),"predicted cost function")
      //val predObjective = objectiveFunction(model,predictedFlows)
      utils.log(s"predicted stat = $predObjective")

      // ! with the form exp(-d/d0), inverse than exp(-beta d)
      val newfitparameter = fitparameter*obsObjective/predObjective
      utils.log(s"param update rate = ${obsObjective/predObjective}")
      val newmodel = model.copy(predictedFlows = predictedFlows, fittedParam = newfitparameter)
      //utils.log(s"avg obs travel distance = $obsObjective")
      //utils.log(s"avg pred travel distance = $predObjective")
      val error = math.abs(predObjective - obsObjective)/obsObjective
      utils.log(s"fit singly constr: error = $error ; iteration in ${System.currentTimeMillis()-t}")
      (newmodel,error)
    }

    val res = Iterator.iterate((initialModel,Double.MaxValue.toDouble))(iterateCostParam).takeWhile(_._2>convergenceThreshold).toSeq.last
    res._1
  }

  /**
    *
    * @param originMasses column vector of origin masses (avoids wrapping/unwrapping in the fitting algorithm)
    * @param destinationMasses column vector of destination masses
    * @param costMatrix cost matrix
    * @param originConstraint constraint at the origin?
    * @return
    */
  def singlyConstrainedFlows(originMasses: Matrix,
                             destinationMasses: Matrix,
                             costMatrix: Matrix,
                             originConstraint: Boolean
                            )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): Matrix = {
    val normalization = (if (originConstraint) SparseMatrix.diagonal((costMatrix %*% destinationMasses).values.flatten) else SparseMatrix.diagonal((costMatrix %*% originMasses).values.flatten)).map(1/_)

    val origin = SparseMatrix.diagonal(originMasses.flatValues)
    val destination = SparseMatrix.diagonal(destinationMasses.flatValues)

    val omat = if (originConstraint) origin %*% normalization else origin
    val dmat = if (originConstraint) destination else destination %*% normalization

    val res = omat%*%costMatrix%*%dmat
    res
  }

}
