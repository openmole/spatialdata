package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.model.urbandynamics.EconomicExchanges.{EconomicExchangesState, updateState}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix}

/**
  * Economic exchanges model adapted from
  *   Cottineau, C., Reuillon, R., Chapron, P., Rey-Coyrehourcq, S., & Pumain, D. (2015). A modular modelling framework for hypotheses testing in the simulation of urbanisation. Systems, 3(4), 348-377.
  *
  *  !!! in many parameter regions, the model does not seem to be valid: only negative wealth updates; or divergent population
  *   -> depends a lot on population settings?
  *  + in some cases converges to DW = 0, DP = 0? (25 time steps? -> limit to 20?)
  *  + "divergence" checking removed (huge values from other models in multi-modeling)
  *
  * @param populationMatrix populationMatrix
  * @param distanceMatrix distanceMatrix
  * @param dates dates
  * @param economicWeight economicWeight
  * @param sizeEffectOnDemand sizeEffectOnDemand
  * @param sizeEffectOnSupply sizeEffectOnSupply
  * @param gravityDecay gravityDecay
  * @param wealthToPopulationExponent wealthToPopulationExponent
  * @param populationToWealthExponent populationToWealthExponent
  */
case class EconomicExchanges(
                            populationMatrix: Matrix,
                            distanceMatrix: Matrix,
                            dates: Array[Double],
                            economicWeight: Double,
                            sizeEffectOnDemand: Double,
                            sizeEffectOnSupply: Double,
                            gravityDecay: Double,
                            wealthToPopulationExponent: Double,
                            populationToWealthExponent: Double
                            ) extends MacroModel {

  override def run: MacroResultFit = EconomicExchanges.run(model = this)

  override def nextStep(state: MacroState, populations: Matrix, distanceMatrix: Matrix): MacroState =
    EconomicExchanges.nextState(this, updateState(this, state.asInstanceOf[EconomicExchangesState], populations, distanceMatrix))

  override def finalTime: Int = dates.length

}


object EconomicExchanges {

  case class EconomicExchangesState(
                                     time: Int,
                                     populations: Matrix,
                                     distanceMatrix: Matrix,
                                     distanceCoefMatrix: Matrix,
                                     wealths: Seq[Double],
                                     gravityPotentials: Matrix
                        ) extends MacroState


  def initialState(model: EconomicExchanges): EconomicExchangesState = {
    import model._
    val populations = model.populationMatrix.getCol(0)
    val wealths = rescaleWealth(populations.flatValues.map(populationToWealth(_, model.populationToWealthExponent)).toSeq, populations.flatValues.toSeq)
    val distanceCoefMatrix = model.distanceMatrix.map(d => math.exp(- d / model.gravityDecay))

    val supplies = populations.flatValues.map(p => supply(p, economicWeight, sizeEffectOnSupply))
    val demands = populations.flatValues.map(p => demand(p, economicWeight, sizeEffectOnDemand))
    val interactionMatrix = interactionPotentialMatrix(supplies.toSeq, demands.toSeq, distanceCoefMatrix)

    EconomicExchangesState(0, populations, model.distanceMatrix, distanceCoefMatrix, wealths, interactionMatrix)
  }

  /**
   * update pop and distance mat without running the model (for coupling with other components)
   * @param state state
   * @param population population
   */
  def updateState(model: EconomicExchanges, state: EconomicExchangesState, population: Matrix, distanceMatrix: Matrix): EconomicExchangesState = {
    val distanceCoefMatrix = distanceMatrix.map(d => math.exp(- d / model.gravityDecay))
    state.copy(populations = population.clone, distanceMatrix = distanceMatrix.clone, distanceCoefMatrix = distanceCoefMatrix)
  }

  def allExcept(n: Int, i: Int): Seq[Int] = (0 until i) ++ (i + 1 until n)
  def outNodes(n:Int, i: Int): Seq[Int] = allExcept(n, i)
  def inNodes(n:Int, i: Int): Seq[Int] = allExcept(n, i)
  def mapNodes(n: Int, f: (Int, Int) => Double): DenseMatrix =
    DenseMatrix(
      Array.tabulate(n, n) {
        (i, j) => if (i != j) f(i, j) else 0.0
      }
    )(DenseMatrix.Real())


  def populationToWealth(population: Double, populationToWealthExponent: Double): Double = math.pow(population, populationToWealthExponent)

  def rescaleWealth(wealth: Seq[Double], population: Seq[Double]): Seq[Double] = {
    val factor = population.sum / wealth.sum
    wealth.map(_ * factor)
  }

  /**
    * Update wealths - returns new wealths and interaction matrix
    * @param state state
    * @param economicMultiplier economicMultiplier
    * @param sizeEffectOnSupply sizeEffectOnSupply
    * @param sizeEffectOnDemand sizeEffectOnDemand
    * @return
    */
  def updatedWealths(state: EconomicExchangesState, economicMultiplier: Double, sizeEffectOnSupply: Double, sizeEffectOnDemand: Double): (Seq[Double], Matrix) = {
    val supplies = state.populations.flatValues.map(p => supply(p, economicMultiplier, sizeEffectOnSupply))
    val demands = state.populations.flatValues.map(p => demand(p, economicMultiplier, sizeEffectOnDemand))

    //utils.log(s"    supplies = ${supplies.toSeq}")
    //utils.log(s"    demands = ${demands.toSeq}")

    val interactionMatrix = interactionPotentialMatrix(supplies.toSeq, demands.toSeq, state.distanceCoefMatrix)

    val balance = exchangeBalances(state.wealths, supplies.toSeq, demands.toSeq, interactionMatrix)

    //utils.log(s"    balances = $balance")

    val w = (state.wealths zip supplies zip demands zip balance).zipWithIndex.map {
      case ((((wealth, supply), demand), b), _) =>
        val newWealth = wealth + supply - demand + b
        //utils.log(s"$wealth + $supply - $demand + $b = $newWealth")
        if (wealth <= 0.0 || newWealth <= 0.0) 0.0 else newWealth
    }
    (w, interactionMatrix)
  }


  /**
    * ! eco multiplier is inverse of original model - for it to be the same unit as innovationWeight, coevolutionWeight in multi-modeling
    *
    * @param population population
    * @param previousWealth previousWealth
    * @param updatedWealth updatedWealth
    * @param deltaT deltaT
    * @param wealthToPopulationExponent wealthToPopulationExponent
    * @param economicMultiplier economicMultiplier
    * @return
    */
  def updatedPopulation(population: Double, previousWealth: Double, updatedWealth: Double,deltaT: Double, wealthToPopulationExponent: Double, economicMultiplier: Double): Double = {
    val wealthpopupdated = wealthToPopulation(updatedWealth, wealthToPopulationExponent)
    val wealthpop = wealthToPopulation(previousWealth, wealthToPopulationExponent)
    val deltaPopulation = deltaT*(wealthpopupdated - wealthpop) * economicMultiplier
    val updatedPopulation = population + deltaPopulation
    //assert(updatedPopulation<1e9,s"divergent population : updatedPopulation $updatedPopulation ; updatedWealth $updatedWealth ; wealthtopop $wealthpop wealthpopupdated $wealthpopupdated")
    if (updatedPopulation >= 0.0) updatedPopulation else 0.0
  }

  def supply(population: Double, economicMultiplier: Double, sizeEffectOnSupply: Double): Double = {
    val supply =  math.pow(population, sizeEffectOnSupply) / economicMultiplier
    assert(!supply.isInfinite,s"Infinite supply : pop $population")
    supply
  }

  def demand(population: Double, economicMultiplier: Double, sizeEffectOnDemand: Double): Double = {
    val demand = math.pow(population, sizeEffectOnDemand) / economicMultiplier
    assert(!demand.isInfinite,s"Infinite demand : pop $population")
    demand
  }

  def exchangeBalances(
                        wealths: Seq[Double],
                        supplies: Seq[Double],
                        demands: Seq[Double],
                        interactionMatrix: Matrix
                      ): Seq[Double] = {
    val transacs = transactions(wealths, supplies, demands, interactionMatrix)

    //utils.log(s"transactions:\n${transacs.values.map(_.mkString("   ")).mkString("\n")}\n")

    val transacToSum = transacs.colSum
    val transacFromSum = transacs.rowSum

    def unsatisfieds: Seq[Double] = for {
      (d, i) <- demands.zipWithIndex
    } yield d - transacToSum(i)

    def unsolds: Seq[Double] =
      for {
        (s, i) <- supplies.zipWithIndex
      } yield s - transacFromSum(i)

    (unsolds zip unsatisfieds).map {
      case (unsold, unsatisfied) => unsatisfied - unsold
    }
  }

  /**
    * transactions between cities
    *
    *  ! changing the interaction function into an exponential makes it fail?
    *
    * @param wealths wealths
    * @param supplies supplies
    * @param demands demands
    * @param interactionMatrix interactions
    * @return
    */
  def transactions(
                    wealths: Seq[Double],
                    supplies: Seq[Double],
                    demands: Seq[Double],
                    interactionMatrix: Matrix
                  ): Matrix = {

    val fromInteractionPotentialSum = interactionMatrix.rowSum
    val toInteractionPotentialSum = interactionMatrix.colSum

    Matrix(
      interactionMatrix.values.zipWithIndex.flatMap{case (row,i) => row.zipWithIndex.map{case (v,j) => (i,j,v)}}.map {
        case (from, to, ip) =>
          if (ip > 0) {
            val fSupply = supplies(from)
            val tDemand = demands(to)
            val toIPSum = toInteractionPotentialSum(to)
            val fromIPSum = fromInteractionPotentialSum(from)
            assert(fSupply >= 0 && tDemand >= 0, s"supply or demand not good, $fSupply $tDemand")

            val normalisedIPFrom = if(fromIPSum > 0) ip / fromIPSum else 0.0
            val normalisedIPTo = if(toIPSum > 0 ) ip / toIPSum else 0.0

            val t = math.min(normalisedIPFrom * fSupply, normalisedIPTo * tDemand)
            assert(!t.isNaN, s"Transacted is NaN: from $from to $to , ip%from : $normalisedIPFrom supplyfrom  $fSupply todemand $tDemand ip%to $normalisedIPTo  fromipsum $fromIPSum toipsum $toIPSum")
            t
          } else 0.0
      }.sliding(wealths.size).toArray
    )(Matrix.defaultImplementation)
  }

  def interactionPotentialMatrix(supplies: Seq[Double], demands: Seq[Double], distanceCoefMatrix: Matrix): Matrix = {
    val iM1 = supplies.toArray
    val iM2 = demands.toArray
    mapNodes(supplies.size, {
      (i, j) => interactionPotential(iM1(i), iM2(j), distanceCoefMatrix.get(i,j))
    })
  }

  def interactionPotential(mass1: Double, mass2: Double, distanceCoef: Double): Double =
    (math.abs(mass1) * math.abs(mass2))*distanceCoef


  def wealthToPopulation(wealth: Double, wealthToPopulationExponent: Double): Double = math.pow(wealth, wealthToPopulationExponent)


  def nextState(
                 model: EconomicExchanges,
                 state: EconomicExchangesState
               ): EconomicExchangesState = {
    utils.log(s"\n----Eco step ${state.time}")
    import model._
    val prevwealth = state.wealths
    val prevpop = state.populations
    val (wealths, interactionMatrix) = updatedWealths(state,economicWeight,sizeEffectOnSupply, sizeEffectOnDemand)

    utils.log(s"Delta W = ${prevwealth.zip(wealths).map{case (w1, w2) => math.abs(w1 - w2)}.sum}")

    val pops = Matrix(state.populations.flatValues.zip(prevwealth).zip(wealths).map {
      case ((p, pw), w) => updatedPopulation(p, pw, w, model.dates(state.time + 1) - model.dates(state.time), wealthToPopulationExponent, economicWeight)
    }, row = false)(Matrix.defaultImplementation)

    utils.log(s"Delta P = ${prevpop.flatValues.zip(pops.flatValues).map{case (p1,p2) => math.abs(p1-p2)}.sum}")


    state.copy(wealths = wealths, populations = pops, time = state.time + 1, gravityPotentials=interactionMatrix)
  }


  def run(model: EconomicExchanges): MacroResultFit = {
    import model._

    val s0: EconomicExchangesState = initialState(model)

    val states = Iterator.iterate((s0, model)){case (s,m) => (nextState(m,s),m)}.takeWhile(_._1.time <= dates.length).toVector
    val simulatedPopulation = Matrix(states.map(_._1.populations.flatValues).toArray.transpose)(Matrix.defaultImplementation)
    MacroResultFit(populationMatrix, simulatedPopulation)
  }

}

