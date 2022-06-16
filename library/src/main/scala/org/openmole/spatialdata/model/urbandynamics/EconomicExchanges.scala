package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix, RealMatrix}

case class EconomicExchanges(
                            populationMatrix: Matrix,
                            dates: Array[Double],
                            sizeEffectOnDemand: Double,
                            sizeEffectOnSupply: Double,
                            economicMultiplier: Double,
                            gravityDecay: Double,
                            wealthToPopulationExponent: Double,
                            populationToWealthExponent: Double
                            ) extends MacroModel {

  override def run: MacroResult = EconomicExchanges.run(model = this)


}


object EconomicExchanges {

  case class MariusState(step: Int,
                         cities: Seq[City],
                         network: FullNetwork,
                         distanceCoefMatrix: DenseMatrix
                        )

  case class City(population: Double, wealth: Double)

  type FullNetwork = Int

  /** Utility methods to manipulate the full network*/
  implicit class FullNetworkDecorator(network: FullNetwork) {
    def allExcept(i: Int): Seq[Int] = (0 until i) ++ (i + 1 until network)
    def outNodes(i: Int): Seq[Int] = allExcept(i)
    def inNodes(i: Int): Seq[Int] = allExcept(i)

    def mapNodes(f: (Int, Int) => Double): DenseMatrix =
      DenseMatrix(
        Array.tabulate(network, network) {
          (i, j) => if (i != j) f(i, j) else 0.0
        }
      )(DenseMatrix.Real())


  }

  def populationToWealth(population: Double, populationToWealthExponent: Double): Double = math.pow(population, populationToWealthExponent)

  def rescaleWealth(wealth: Seq[Double], population: Seq[Double]): Seq[Double] = {
    val factor = population.sum / wealth.sum
    wealth.map(_ * factor)
  }


  def run(model: EconomicExchanges): MacroResult = {

    import model._



    def updatedWealths(state: MariusState): Seq[Double] = {
      val supplies = state.cities.map(c => supply(c.population))
      val demands = state.cities.map(c => demand(c.population))

      (state.cities zip supplies zip demands zip exchangeBalances(state.cities, state.network, supplies, demands, state.distanceCoefMatrix)).zipWithIndex.map {
        case ((((city, supply), demand), b), _) =>
          val newWealth = city.wealth + supply - demand + b
          if (city.wealth <= 0.0 || newWealth <= 0.0) 0.0 else newWealth
      }
    }

    def updatedPopulation(city: City, updatedWealth: Double,deltaT: Double): Double = {
      //assert(updatedWealth >= 0, s"Negative wealth before conversion toPop $updatedWealth")
      // negative wealth can not happen (cf updatedWealth function)
      val wealthpopupdated = wealthToPopulation(updatedWealth)
      val wealthpop = wealthToPopulation(city.wealth)
      val deltaPopulation = deltaT*(wealthpopupdated - wealthpop) / economicMultiplier
      val updatedPopulation = city.population + deltaPopulation
      //assert(updatedPopulation >= 0, s"Negative population $updatedPopulation")
      //this does not make sense, take 0 if negative. Then some conservation equations may not hold ?
      //assert(!updatedPopulation.isInfinite,s"Infinite pop $updatedPopulation")
      assert(updatedPopulation<1e9,s"divergent population : updatedWealth $updatedWealth wealthtopop $wealthpop wealthpopupdated $wealthpopupdated")

      if (updatedPopulation >= 0.0) updatedPopulation else 0.0
    }

    def supply(population: Double): Double = {
      val supply = economicMultiplier * math.pow(population, sizeEffectOnSupply)
      assert(!supply.isInfinite,s"Infinite supply : pop $population")
      supply
    }

    def demand(population: Double): Double = {
      val demand = economicMultiplier * math.pow(population, sizeEffectOnDemand)
      assert(!demand.isInfinite,s"Infinite demand : pop $population")
      demand
    }

    def exchangeBalances(
                          cities: Seq[City],
                          network: FullNetwork,
                          supplies: Seq[Double],
                          demands: Seq[Double],
                          distanceCoefMatrix: DenseMatrix
                        ): Seq[Double] = {
      val transacs = transactions(cities, network, supplies, demands, distanceCoefMatrix)
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

    def transactions(
                      cities: Seq[City],
                      network: FullNetwork,
                      supplies: Seq[Double],
                      demands: Seq[Double],
                      distanceCoefMatrix: DenseMatrix
                    ): DenseMatrix = {

      val interactionMatrixValue = interactionPotentialMatrix(supplies, demands, network, distanceCoefMatrix)
      val fromInteractionPotentialSum = interactionMatrixValue.rowSum
      val toInteractionPotentialSum = interactionMatrixValue.colSum

      DenseMatrix(
        interactionMatrixValue.values.zipWithIndex.flatMap{case (row,i) => row.zipWithIndex.map{case (v,j) => (i,j,v)}}.map {
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
        }.sliding(cities.size).toArray
      )(DenseMatrix.Real())
    }

    def interactionPotentialMatrix(supplies: Seq[Double], demands: Seq[Double], network: FullNetwork, distanceCoefMatrix: DenseMatrix): DenseMatrix = {
      val iM1 = supplies.toArray
      val iM2 = demands.toArray
      network.mapNodes {
        (i, j) => interactionPotential(iM1(i), iM2(j), distanceCoefMatrix.get(i,j))
      }
    }

    def interactionPotential(mass1: Double, mass2: Double, distanceCoef: Double): Double =
      (math.abs(mass1) * math.abs(mass2))*distanceCoef



    def wealthToPopulation(wealth: Double): Double = math.pow(wealth, wealthToPopulationExponent)


    def nextState(state: MariusState): MariusState = {
      def updatedCities: Seq[City] =
        (state.cities zip updatedWealths(state)).map {
          case (city, updatedWealth) =>
            city.copy(
              wealth = updatedWealth,
              population = updatedPopulation(city, updatedWealth, dates(state.step + 1) - dates(state.step))
            )
        }
      //println(state.step)
      state.copy(cities = updatedCities, step = state.step + 1)
    }

    val initialState: MariusState = MariusState()

    val states = Iterator.iterate(initialState)(nextState).takeWhile(_.step <= dates.length).toVector
    val simulatedPopulation = DenseMatrix(states.map(_.cities.toArray.map(_.population)).toArray.transpose)(DenseMatrix.Real())
    MacroResult(populationMatrix, simulatedPopulation)
  }

}

