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
                                        fittedParam: Double = 1.0,
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
    val (origin,destination) = (SparseMatrix(model.originValues.values.flatten.toArray,false),SparseMatrix(model.destinationValues.values.flatten.toArray,false))

    println(s"origin: ${origin.nrows}x${origin.ncols}")
    println(s"destination: ${destination.nrows}x${destination.ncols}")
    println(s"dmat: ${model.distances.nrows}x${model.distances.ncols}")

    val obsObjective = objectiveFunction(model,model.observedFlows)

    /**
      * State is (model including cost function, current parameter value, epsilon)
      * @param state
      * @return
      */
    def iterateCostParam(state: (SinglyConstrainedSpIntModel,Double)):  (SinglyConstrainedSpIntModel,Double) = {
      val t = System.currentTimeMillis()
      val model = state._1
      val fitparameter = model.fittedParam
      val currentCostMatrix = model.distances.map(model.costFunction(_,fitparameter))
      val predictedFlows = singlyConstrainedFlows(origin,destination,currentCostMatrix,originConstraint)
      val predObjective = objectiveFunction(model,predictedFlows)
      // ! with the form exp(-d/d0), inverse than exp(-beta d)
      val newfitparameter = fitparameter*obsObjective/predObjective
      val newmodel = model.copy(predictedFlows = predictedFlows, fittedParam = newfitparameter)
      //utils.log(s"avg obs travel distance = $obsObjective")
      //utils.log(s"avg pred travel distance = $predObjective")
      val error = math.abs(predObjective - obsObjective)/obsObjective
      utils.log(s"fit singly constr: error = $error ; iteration in ${System.currentTimeMillis()-t}")
      (newmodel,error)
    }
    val initialModel = model.copy(predictedFlows=singlyConstrainedFlows(origin,destination,model.distances.map(model.costFunction(_,initialValue)),originConstraint),
                                  fittedParam = initialValue)
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
    utils.log("Singly constrained flows")
    val (normalization,tn) = utils.withTimer[Double,Matrix]{_ => (if (originConstraint) costMatrix %*% destinationMasses else costMatrix %*% originMasses).map(1/_)}(0.0)
    println(tn)
    // FIXME add sparse option here - element wise multiplication later - should be able to build in O(elem) in a sparse way
    val originnormraw = (originMasses*normalization).flatValues
    val originraw = originMasses.flatValues
    val destinationnormraw = (destinationMasses*normalization).flatValues
    val destinationraw = destinationMasses.flatValues
    // FIXME this shouldnt take so much time to duplicate column vectors - optimize
    // !!! takes forever to duplicate - as reconstructing full sparse mat, totally inefficient
    val (omat,to) = utils.withTimer[Double,Matrix]{_ => if (originConstraint) SparseMatrix(Array.fill(destinationMasses.nrows)(originnormraw).transpose) else SparseMatrix(Array.fill(destinationMasses.nrows)(originraw).transpose)}(0.0)
    println(s"origin mat: $to")
    val dmat = if (originConstraint) SparseMatrix(Array.fill(originMasses.nrows)(destinationraw)) else SparseMatrix(Array.fill(originMasses.nrows)(destinationnormraw))
    val (res,t) = utils.withTimer[Double,Matrix]{_ => omat*dmat*costMatrix}(0.0) // x2 cost overhead
    println(s"res: $t")
    res
  }





}
