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
  // FIXME this is not the most generic representation: cf MultiMode version
  def distances: Matrix
  def originValues: SpatialField[Double]
  def destinationValues: SpatialField[Double]
  def predictedFlows: Matrix


  def flowsAsLinks: Seq[Link] = originValues.keys.zipWithIndex.map{case (oi,i) => destinationValues.keys.zipWithIndex.map{
    case (dj,j) => Link(Node(oi),Node(dj),weight = observedFlows.get(i,j))
  }.toSeq}.toSeq.flatten


}

object SpatialInteractionModel {

  val empty = new SpatialInteractionModel {
    def observedFlows = EmptyMatrix()
    def distances = EmptyMatrix()
    def originValues: SpatialField[Double] = Map.empty
    def destinationValues: SpatialField[Double] = Map.empty
    def predictedFlows: Matrix = EmptyMatrix()
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
