package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.utils.math.Statistics

trait MesoState {
  def time: Int
  def populationGrid: Vector[Vector[Double]]

  def morphology: (Double,Double,Double,Double,Double) = {
    val pop = populationGrid.map{_.toArray}.toArray
    val s = Statistics.slope(pop)
    (GridMorphology.moran(pop),GridMorphology.distanceMean(pop),Statistics.entropy(pop),s._1,s._2)
  }

}