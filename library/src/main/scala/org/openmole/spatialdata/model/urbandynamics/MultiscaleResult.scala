package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.Statistics


/**
  * computed indicators for a multiscale simu
 *
  * @param rawStates
  * @param macroPopulations
  * @param macroClosenesses
  * @param macroAccessibilities
  * @param mesoMorans
  * @param mesoDistances
  * @param mesoEntropy
  * @param mesoSlopes
  * @param mesoSlopeRsquared
  * @param mesoCongestedFlows
  */
case class MultiscaleResult(
                             rawStates: Vector[MultiscaleState],
                             // all flattened indics
                             macroPopulations: Vector[Double],
                             macroClosenesses: Vector[Double],
                             macroAccessibilities: Vector[Double],
                             mesoMorans: Vector[Double],
                             mesoDistances: Vector[Double],
                             mesoEntropy: Vector[Double],
                             mesoSlopes: Vector[Double],
                             mesoSlopeRsquared: Vector[Double],
                             mesoCongestedFlows: Vector[Double],
                             mesoMissingPopulations: Vector[Double],
                             fullTrajectories: Boolean,
                             timesteps: Int,
                             cities: Int
                           ) {
  def asArrayTuple =  (macroPopulations.toArray,macroClosenesses.toArray,macroAccessibilities.toArray,
    mesoMorans.toArray,mesoDistances.toArray,mesoEntropy.toArray,mesoSlopes.toArray,mesoSlopeRsquared.toArray,
    mesoCongestedFlows.toArray,mesoMissingPopulations.toArray
  )


  /**
    * Returns: Macro : hierarchy of populations, average closeness ? -> depends only on dG, average access ? (how the system is balanced -> idem)
    *  Meso : limit sprawl but also aggregation (not too dense)
    *   could try to minimize congestion? ~ no traffic model at the meso scale - would not make much sense
    */
  def summaryIndicators: (Double,Double,Double) = {
    // whatever the initial hierarchy (constant in the synthetic context), should be minimized
    val popMacroHierarchy = Statistics.slope(macroPopulations.takeRight(cities).toArray)._1
    val averageMesoAggregation = mesoSlopes.takeRight(cities).sum / cities.toDouble
    val averageMesoDistance = mesoDistances.takeRight(cities).sum / cities.toDouble
    (popMacroHierarchy,averageMesoAggregation,averageMesoDistance)
  }



}

object MultiscaleResult {

  def apply(rawStates: Vector[MultiscaleState],fulltrajs: Boolean): MultiscaleResult = {
    val states: Vector[MultiscaleState] = if (fulltrajs) rawStates else rawStates.take(1)++rawStates.takeRight(1)
    val morphologies: Vector[(Double,Double,Double,Double,Double)] = states.flatMap{_.mesoStates.map{_.morphology}}
    val macroIndics: Vector[(Vector[Double],Vector[Double],Vector[Double])] = states.map{_.macroState.indicators}
    MultiscaleResult(
      states,
      macroIndics.flatMap(_._1),
      macroIndics.flatMap(_._2),
      macroIndics.flatMap(_._3),
      morphologies.map(_._1),
      morphologies.map(_._2),
      morphologies.map(_._3),
      morphologies.map(_._4),
      morphologies.map(_._5),
      states.flatMap(_.macroState.congestedFlows),
      states.flatMap(_.mesoStates.map(_.missingPopulation)),
      fulltrajs,
      rawStates.length,
      macroIndics.head._1.length
    )
  }



}
