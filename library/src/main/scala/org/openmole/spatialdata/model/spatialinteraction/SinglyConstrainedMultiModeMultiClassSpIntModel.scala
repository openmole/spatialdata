package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.{Matrix, SparseMatrix}
import org.openmole.spatialdata.vector.SpatialField


/**
  *
  * @param modesObservedFlows
  * @param modesDistances
  * @param classesObservedFlows
  * @param originValues
  * @param destinationValues
  * @param fittedParams
  * @param costFunction
  * @param segmentPredictedFlows
  */
case class SinglyConstrainedMultiModeMultiClassSpIntModel (
                                                  modesObservedFlows: Array[Matrix],
                                                  modesDistances: Array[Matrix],
                                                  classesObservedFlows: Array[Matrix],
                                                  originValues: SpatialField[Double],
                                                  destinationValues: SpatialField[Double],
                                                  fittedParams: Array[Double] = Array.empty[Double],
                                                  costFunction: (Double,Double)=> Double = {case (d,d0) => math.exp(-d / d0)},
                                                  segmentPredictedFlows: Array[Array[Matrix]] = Array.empty[Array[Matrix]]
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

  override def predictedFlows: Matrix = segmentPredictedFlows.map(_.reduce(Matrix.msum)).reduce(Matrix.msum)

  /**
    * rq: the generic function does not make sense as it fits itself in the end?
    * @return
    */
  override def fit(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SpatialInteractionModel => FittedSpIntModel = {
    s: SpatialInteractionModel => s match {
      case m: SinglyConstrainedMultiModeSpIntModel =>
        SinglyConstrainedMultiModeSpIntModel.fitSinglyConstrainedMultiModeSpIntModel(m,
          m.fittedParams,
          SinglyConstrainedMultiModeSpIntModel.averageTripLength,
          true,
          0.01)
      case _ => throw new IllegalArgumentException("Can not fit other type of models")
    }
  }


}

