package org.openmole.spatialdata.model.spatialinteraction.synthetic

import org.openmole.spatialdata.model.spatialinteraction.{FlowsGenerator, SpatialInteractionData, SpatialInteractionModel}
import org.openmole.spatialdata.utils.math.Matrix
import org.openmole.spatialdata.vector.{FieldGenerator, Point}

import scala.util.Random


/**
  * Generate a synthetic urban system with O/D flows produced by a spatial interaction model
  *
  * @param originFieldGenerator field generator for origin
  * @param destinationFieldGenerator field generator for destination
  * @param distanceMatrix distance matrix
  * @param costFunction cost function (applied to the matrix)
  * @param originTransformation transformation of the origin field
  * @param destinationTransformation transformation of the destination field
  * @param flowsFunction create flows from fields and cost matrix - note that it could only take fields and include cost matrix computation, but better to stay generic at this higher level
  */
case class SyntheticFlowsGenerator(
                                  originFieldGenerator: FieldGenerator[Double],
                                  destinationFieldGenerator: FieldGenerator[Double],
                                  distanceMatrix: (Seq[Point],Seq[Point]) => Matrix,
                                  costFunction: Double => Double,
                                  originTransformation: Array[Double] => Double,
                                  destinationTransformation: Array[Double] => Double,
                                  flowsFunction: (Seq[Double],Seq[Double],Matrix) => Matrix
                                  ) extends FlowsGenerator {
  override def generateFlows(implicit rng: Random): SpatialInteractionModel =
    SyntheticFlowsGenerator.syntheticFlows(originFieldGenerator,destinationFieldGenerator,distanceMatrix,costFunction,originTransformation,destinationTransformation,flowsFunction,rng)
}


object SyntheticFlowsGenerator {


  /**
    * Generic function for synthetic flows
    *
    * @param originFieldGenerator generator for origin field
    * @param destinationFieldGenerator generator for destination field
    * @param distanceMatrix distance matrix
    * @param costFunction cost function applied to the distance matrix
    * @param originTransformation transformation of the origin field
    * @param destinationTransformation transformation of the destination field
    * @param rng rng
    * @return
    */
  def syntheticFlows(originFieldGenerator: FieldGenerator[Double],
                     destinationFieldGenerator: FieldGenerator[Double],
                     distanceMatrix: (Seq[Point],Seq[Point]) => Matrix,
                     costFunction: Double => Double,
                     originTransformation: Array[Double] => Double,
                     destinationTransformation: Array[Double] => Double,
                     flowsFunction: (Seq[Double],Seq[Double],Matrix) => Matrix,
                     rng: Random
                    ): SpatialInteractionModel = {
    val originField = originFieldGenerator.generateField(rng)
    val (originPoints,originMasses): (Seq[Point],Seq[Double]) = originField.transform{case (_,a) => originTransformation(a)}.toSeq.unzip
    val destinationField = destinationFieldGenerator.generateField(rng)
    val (destinationPoints,destinationMasses): (Seq[Point],Seq[Double]) = destinationField.transform{case (_,a) => destinationTransformation(a)}.toSeq.unzip
    val distances: Matrix = distanceMatrix(originPoints,destinationPoints)
    val costMatrix: Matrix = distances.map(costFunction)
    val flows = flowsFunction(originMasses, destinationMasses, costMatrix)
    SpatialInteractionData(flows, distances, originField, destinationField)
  }

}

