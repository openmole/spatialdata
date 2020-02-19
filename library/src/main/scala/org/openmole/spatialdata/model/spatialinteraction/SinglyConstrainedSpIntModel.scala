package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{EmptyMatrix, Matrix}
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

  override def fit: SpatialInteractionModel => SpatialInteractionModel = {s => s}

  /*{
    _ match {
      case m:  SinglyConstrainedSpIntModel => SinglyConstrainedSpIntModel.fitSinglyConstrainedSpIntModel(m)
    }
  } */

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
    val phitot = phi.sum
    (phi.map(_ / phitot)*model.distances).sum
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

    val (origin,destination) = (Matrix(model.originValues.values.flatten.toArray,false),Matrix(model.destinationValues.values.flatten.toArray,false))

    println(s"origin: ${origin.nrows}x${origin.ncols}")
    println(s"destination: ${destination.nrows}x${destination.ncols}")
    println(s"dmat: ${model.distances.nrows}xx${model.distances.ncols}")

    /**
      * State is (model including cost function, current parameter value, epsilon)
      * @param state
      * @return
      */
    def iterateCostParam(state: (SinglyConstrainedSpIntModel,Double)):  (SinglyConstrainedSpIntModel,Double) = {
      val model = state._1
      val fitparameter = model.fittedParam
      val currentCostMatrix = model.distances.map(model.costFunction(_,fitparameter))
      val predictedFlows = singlyConstrainedFlows(origin,destination,currentCostMatrix,originConstraint)
      val predObjective = objectiveFunction(model,predictedFlows)
      val obsObjective = objectiveFunction(model,model.observedFlows)
      // ! with the form exp(-d/d0), inverse than exp(-beta d)
      val newfitparameter = fitparameter*obsObjective/predObjective
      val newmodel = model.copy(predictedFlows = predictedFlows, fittedParam = newfitparameter)
      //utils.log(s"avg obs travel distance = $obsObjective")
      //utils.log(s"avg pred travel distance = $predObjective")
      val error = math.abs(predObjective - obsObjective)/obsObjective
      //utils.log("fit singly constr: error = "+error)
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
    val normalization = (if (originConstraint) costMatrix %*% destinationMasses else costMatrix %*% originMasses).map(1/_)
    // FIXME add sparse option here - element wise multiplication later - should be able to build in O(elem) in a sparse way
    val omat = if (originConstraint) Matrix(Array.fill(destinationMasses.nrows)((originMasses*normalization).values.flatten).transpose) else Matrix(Array.fill(destinationMasses.nrows)(originMasses.values.flatten).transpose)
    val dmat = if (originConstraint) Matrix(Array.fill(originMasses.nrows)(destinationMasses.values.flatten)) else Matrix(Array.fill(originMasses.nrows)((destinationMasses*normalization).values.flatten))
    omat*dmat*costMatrix
  }





}
