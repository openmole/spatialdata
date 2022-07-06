package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.network.{Link, Node}
import org.openmole.spatialdata.utils.math.{EmptyMatrix, Matrix}
import org.openmole.spatialdata.vector.SpatialField


/**
  * Most abstract representation of a spatial interaction model.
  *
  *  Wilson, A. G. (1971). A family of spatial interaction models, and associated developments. Environment and Planning A, 3(1), 1-32.
  *
  *  It must (i) be spatial -> representation of impedance in space; (ii) dealing with interacting entities -> Origin/Destination spatial fields/flows
  *  (iii) be a model -> observed and predicted flows.
  *
  *  The most generic functional signature would be something like
  *     ImpedanceField => SpatialField => SpatialField => SpatialFlows => SpatialFlows
  *
  *  We implement it here with a distance matrix to represent space.
  *  Note: distances/flows may be multi-dimensional (cf Multimodal Quant model)? -> we can superpose several models
  *
  */
trait SpatialInteractionModel {

  def observedFlows: Matrix
  // ! this is not the most generic representation: cf MultiMode version: multiple matrices
  def distances: Matrix
  // dual representation depending on implementation: either store distances, or distance weights (in the case of sparsing in particular)
  def distanceWeights: Matrix
  def originValues: SpatialField[Double]
  def destinationValues: SpatialField[Double]
  def predictedFlows: Matrix


  def flowsAsLinks: Seq[Link] = originValues.keys.zipWithIndex.map{case (oi,i) => destinationValues.keys.zipWithIndex.map{
    case (dj,j) => Link(Node(oi),Node(dj),weight = observedFlows.get(i,j))
  }.toSeq}.toSeq.flatten


}

object SpatialInteractionModel {

  val empty: SpatialInteractionModel = new SpatialInteractionModel {
    override def observedFlows: Matrix = EmptyMatrix()
    override def distances: Matrix = EmptyMatrix()
    override def distanceWeights: Matrix = EmptyMatrix()
    override def originValues: SpatialField[Double] = Map.empty
    override def destinationValues: SpatialField[Double] = Map.empty
    override def predictedFlows: Matrix = EmptyMatrix()
  }
  //implicit class asFunctionDecorator(m: SpatialInteractionModel){
  //   def asFunction: Matrix => SpatialField => SpatialField => Matrix => Matrix = ???
  //}

  /*
  // not useful: use SpatialInteractionData
  def apply(observed: Matrix,dists: Matrix, origin: SpatialField, destination: SpatialField): SpatialInteractionModel =
    new SpatialInteractionModel{
      def observedFlows =observed
      def distances = dists
      def originValues = origin
      def destinationValues = destination
      def predictedFlows = EmptyMatrix()
    }
    */


}
