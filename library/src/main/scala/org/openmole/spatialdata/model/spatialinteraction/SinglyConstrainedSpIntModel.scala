package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.model.spatialinteraction.SinglyConstrainedSpIntModel.averageTripLength
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{EmptyMatrix, Matrix, SparseMatrix}
import org.openmole.spatialdata.vector.SpatialField

/**
  *
  * @param observedFlows
  * @param distances
  * @param costFunction The function on which parameter optimization is done
  * @param originValues
  * @param destinationValues
  * @param predictedFlows
  */
case class SinglyConstrainedSpIntModel(
                                        observedFlows: Matrix,
                                        distances: Matrix,
                                        originValues: SpatialField[Double],
                                        destinationValues: SpatialField[Double],
                                        costFunction: (Double,Double)=> Double = {case (d,d0) => math.exp(-d / d0)},
                                        fittedParams: Double = 1.0,
                                        predictedFlows: Matrix = EmptyMatrix()
                                      ) extends FittedSpIntModel {

  /**
    * rq: the generic function does not make sense as it fits itself in the end?
    * @return
    */
  override def fit: SpatialInteractionModel => SpatialInteractionModel = {
    s => s match {
      case m: SinglyConstrainedSpIntModel => SinglyConstrainedSpIntModel.fitSinglyConstrainedSpIntModel(m, averageTripLength, 1.0, true, 0.01)
      case _ => throw new IllegalArgumentException("Can not fit other type of models")
    }
  }


}


object SinglyConstrainedSpIntModel {

  /**
    * copy from another model, e.g. synthetic or real unfitted flows
    * @param model
    * @return
    */
  def apply(model: SpatialInteractionModel): SinglyConstrainedSpIntModel = SinglyConstrainedSpIntModel(model.observedFlows,model.distances,model.originValues,model.destinationValues)

  /**
    * one possible statistic to adjust the model
    * @param model
    * @param phi
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
    * TODO run with sparse matrices for perf (after having benchmarked sparse mat impls)
    * @param model Model to fir
    * @param objectiveFunction statistic compared between the two models
    * @param originConstraint constraints at the origin?
    * @param convergenceThreshold
    * @return
    */
  def fitSinglyConstrainedSpIntModel(model: SinglyConstrainedSpIntModel,
                                     objectiveFunction: (SinglyConstrainedSpIntModel,Matrix) => Double = averageTripLength,
                                     initialValue: Double = 1.0,
                                     originConstraint: Boolean = true,
                                     convergenceThreshold: Double = 0.01
                                    ): SinglyConstrainedSpIntModel = {

    // ! force a SparseMatrix here
    val origin = utils.timerLog[Unit,Matrix](_ => SparseMatrix(model.originValues.values.flatten.toArray,false),(),"origin column matrix")
    val destination = utils.timerLog[Unit,Matrix](_ => SparseMatrix(model.destinationValues.values.flatten.toArray,false),(),"destination column matrix")
    println(s"origin column mat = ${origin}")

    val obsObjective = utils.timerLog[Unit,Double](_ => objectiveFunction(model,model.observedFlows),(),"objective cost function")
    utils.log(s"observed stat = $obsObjective")

    val initialFlows = utils.timerLog[Matrix](singlyConstrainedFlows(origin,destination,model.distances.map(model.costFunction(_,initialValue)),originConstraint),"initial flows")

    val initialModel = model.copy(predictedFlows=initialFlows, fittedParam = initialValue)

    /**
      * State is (model including cost function, current parameter value, epsilon)
      * @param state
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
    * @param costMatrix
    * @param originConstraint
    * @return
    */
  def singlyConstrainedFlows(originMasses: Matrix,
                             destinationMasses: Matrix,
                             costMatrix: Matrix,
                             originConstraint: Boolean
                            ): Matrix = {
    //utils.log("Singly constrained flows")
    //val normalization = utils.timerLog[Matrix]((if (originConstraint) SparseMatrix.diagonal((costMatrix %*% destinationMasses).flatValues) else SparseMatrix.diagonal((costMatrix %*% originMasses).flatValues)).map(1/_),"normalisation")
    // !!! do not use flatValues as some values can be zero after mult by cost
    //val normalization = (if (originConstraint) SparseMatrix.diagonal((costMatrix %*% destinationMasses).flatValues) else SparseMatrix.diagonal((costMatrix %*% originMasses).flatValues)).map(1/_)
    //val normalization = utils.timerLog[Unit,Matrix](_=> (if (originConstraint) SparseMatrix.diagonal((costMatrix %*% destinationMasses).values.flatten) else SparseMatrix.diagonal((costMatrix %*% originMasses).values.flatten)).map(1/_),(),"normalisation")
    val normalization = (if (originConstraint) SparseMatrix.diagonal((costMatrix %*% destinationMasses).values.flatten) else SparseMatrix.diagonal((costMatrix %*% originMasses).values.flatten)).map(1/_)

    val origin = SparseMatrix.diagonal(originMasses.flatValues)
    val destination = SparseMatrix.diagonal(destinationMasses.flatValues)

    //val originnorm = SparseMatrix.diagonal((originMasses*normalization).flatValues)
    //val destinationnorm = SparseMatrix.diagonal((destinationMasses*normalization).flatValues)
    /*
    // FIXME this shouldnt take so much time to duplicate column vectors - optimize
    // !!! takes forever to duplicate - as reconstructing full sparse mat, totally inefficient
    val (omat,to) = utils.withTimer[Double,Matrix]{_ => if (originConstraint) SparseMatrix(Array.fill(destinationMasses.nrows)(originnormraw).transpose) else SparseMatrix(Array.fill(destinationMasses.nrows)(originraw).transpose)}(0.0)
    println(s"origin mat: $to")
    val dmat = if (originConstraint) SparseMatrix(Array.fill(originMasses.nrows)(destinationraw)) else SparseMatrix(Array.fill(originMasses.nrows)(destinationnormraw))
    // FIXME ! also inefficient to multiply full sparse mats => implement cols/rows times scalar
    val (res,t) = utils.withTimer[Double,Matrix]{_ => omat*dmat*costMatrix}(0.0) // x2 cost overhead
    println(s"res: $t")
    res
    // => diagonal sparse matrices!
    */

    //val omat = utils.timerLog[Matrix](if (originConstraint) origin %*% normalization else origin,"omat")
    //val dmat = utils.timerLog[Matrix](if (originConstraint) destination else destination %*% normalization, name="dmat")
    val omat = if (originConstraint) origin %*% normalization else origin
    val dmat = if (originConstraint) destination else destination %*% normalization

    //val res = utils.timerLog[Matrix](omat%*%costMatrix%*%dmat,name = "full flows")
    val res = omat%*%costMatrix%*%dmat
    res
  }

}
