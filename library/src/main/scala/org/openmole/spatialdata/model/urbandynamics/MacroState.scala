package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.Matrix


/**
  * Macroscopic state
  *  rq : coordinates are not useful once the distance matrix has been computed ?
  */
trait MacroState {
  def time: Int
  def populations: Vector[Double]
  def distanceMatrix: Matrix

  /**
    * Compute macroscopic indicators : population, closeness and accessibility
    * @return
    */
  def indicators: (Vector[Double],Vector[Double],Vector[Double]) = {
    val pop = populations
    val dmat = distanceMatrix
    val ptot = populations.sum
    (pop,dmat.values.map(r => r.sum/r.length).toVector,dmat.values.map(r=> r.zip(pop).map{case (e,p)=> e*p/ptot}.sum / r.length).toVector)
  }

}




/*
object MacroState {
  def initialSyntheticState(): MacroState = MacroState(0)
}
*/
