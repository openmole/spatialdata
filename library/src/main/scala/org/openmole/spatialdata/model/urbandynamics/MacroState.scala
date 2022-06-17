package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.Matrix



trait MacroState {
  def time: Int
  def populations: Matrix
  def distanceMatrix: Matrix
}

/**
 * Macroscopic state
 *   rq : coordinates are not useful once the distance matrix has been computed ?
 *
 * @param time time step
 * @param populations populations as column vector
 * @param distanceMatrix distance matrix  - not cached as evolves at each step - must be from the beginning a generalized distance matrix ? (! update rule ?)
 */
case class MacroStateGen(
  time: Int,
  populations: Matrix,
  distanceMatrix: Matrix
) extends MacroState {

  /**
    * Compute macroscopic indicators : population, closeness and accessibility
    * @return
    */
  def indicators: (Vector[Double],Vector[Double],Vector[Double]) = {
    val pop = populations.flatValues.toVector
    val dmat = distanceMatrix
    val ptot = populations.sum
    (pop,dmat.values.map(r => r.sum/r.length).toVector,dmat.values.map(r=> r.zip(pop).map{case (e,p)=> e*p/ptot}.sum / r.length).toVector)
  }

}



