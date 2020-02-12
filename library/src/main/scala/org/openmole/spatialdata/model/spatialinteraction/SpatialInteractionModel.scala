package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.Matrix
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
  def distances: Matrix
  def originValues: SpatialField
  def destinationValues: SpatialField
  def predictedFlows: Matrix



}

object SpatialInteractionModel {

  //implicit class asFunctionDecorator(m: SpatialInteractionModel){
  //   def asFunction: Matrix => SpatialField => SpatialField => Matrix => Matrix = ???
  //}

}
