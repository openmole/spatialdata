package org.openmole.spatialdata.model.urbandynamics


import org.openmole.spatialdata.model.urbandynamics.Innovation.InnovationState
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{DenseMatrix, EmptyMatrix, Matrix, Statistics, Stochastic}
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.vector.measures.Spatstat
import org.openmole.spatialdata.vector.synthetic.RandomPointsGenerator

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


/**
  *
  * Generalization of the Favaro-Pumain model for urban systems dynamics based on the diffusion of innovation
  *
  *   Favaro, J. M., & Pumain, D. (2011). Gibrat Revisited: An Urban Growth Model Incorporating Spatial Interaction and Innovation Cycles. Geographical Analysis, 43(3), 261-286.
  *   Raimbault, J. (2020). A model of urban evolution based on innovation diffusion. Artifical Life 2020 Proceedings.
  *
  *
  * @param populationMatrix        Real population matrix
  * @param distanceMatrix          Distance matrix
  * @param dates                   Dates
  * @param rng                     Model has its own rng
  * @param growthRate              Gibrat growth rate
  * @param innovationWeight        weight of innovation induced growth rate
  * @param gravityDecay            Decay of gravity interaction
  * @param innovationDecay         Decay of innovation diffusion
  */
case class Innovation(
                       populationMatrix: Matrix,
                       distanceMatrix: Matrix,
                       dates: Array[Double],
                       rng : Random,
                       growthRate: Double,
                       innovationWeight: Double,
                       gravityDecay: Double,
                       innovationDecay: Double,
                       newInnovation: (Seq[Double],Seq[Double],Seq[Seq[Double]]) => (Boolean,Seq[Double],Seq[Seq[Double]]),
                       initialInnovationUtility: Double
                     ) extends MacroModel {

  /**
    * run for model calibration
    * @return
    */
  override def run: MacroResultFit = Innovation.run(this).macroResult


  override def nextStep(state: MacroState, populations: Matrix, distanceMatrix: Matrix): MacroState = {
    val updatedState = Innovation.updateState(state.asInstanceOf[InnovationState], populations, distanceMatrix)
    Innovation.nextState(this, updatedState)
  }

  override def finalTime: Int = dates.length

  override def toString: String = "Innovation model with parameters"+
    "\n\tgrowthRate = "+growthRate+"\n\tinnovationWeight = "+innovationWeight+"\n\tgravityDecay = "+gravityDecay+
    "\n\tinnovationDecay = "+innovationDecay

}


object Innovation {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering


  case class InnovationState(
                            time: Int,
                            populations: Matrix,
                            distanceMatrix: Matrix,
                            innovations: Seq[Matrix],
                            utilities: Seq[Double],
                            flows: Matrix
                            ) extends MacroState

  /**
    * "Semi-synthetic" initial state: from model pop and distance param, one single archaic techno
    * @param model model
    * @return
    */
  def initialState(model: Innovation): InnovationState = {

    import model._

    val innovationUtilities: ArrayBuffer[Double] = new ArrayBuffer[Double]
    innovationUtilities.append(model.initialInnovationUtility)
    val innovationProportions: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]
    val archaicTechno =  DenseMatrix.zeros(populationMatrix.nrows, populationMatrix.ncols)
    archaicTechno.setMSubmat(0, 0, Array.fill(populationMatrix.nrows)(Array(1.0)))
    innovationProportions.append(archaicTechno)
    val diffusedInnovs = innovationProportions.toSeq

    // do not compute interaction potentials at initial state (in practice not realised?) - filter empty matrix in indic computation?
    // -> better compute for consistency
    val gravityDistanceWeights = distanceMatrix.map { d => Math.exp(-d / gravityDecay) }
    val potsgravity = gravityPotentials(diffusedInnovs, Array(1.0), populationMatrix.getCol(0).flatValues, gravityDistanceWeights, 0)

    InnovationState(time=0, populationMatrix.getCol(0), distanceMatrix, diffusedInnovs, innovationUtilities.toSeq, flows = potsgravity)

  }

  def updateState(state: InnovationState, populations: Matrix, distanceMatrix: Matrix): InnovationState = {
    state.copy(populations = populations.clone, distanceMatrix = distanceMatrix.clone)
  }


  sealed trait InnovationUtilityDistribution
  case class InnovationUtilityNormalDistribution() extends InnovationUtilityDistribution
  case class InnovationUtilityLogNormalDistribution() extends InnovationUtilityDistribution

  /**
    * Specific result, includes indicators
    * @param macroResult populations
    * @param innovationUtilities utilities
    * @param innovationShares shares
    */
  case class InnovationResult(
                               macroResult: MacroResultFit,
                               innovationUtilities: Seq[Double],
                               innovationShares: Seq[Matrix],
                               gravityPotentials: Seq[Matrix]
                             ) {

    /**
      * Utility averaged over cities, innovations and time, weighted by population and innovation share
      *
      * 1 / T \sum_t {\sum_i,c (P_i(t) /P_tot(t)) * delta_{i,c,t} * u_c}
      *
      * @return
      */
    def averageUtility: Double = {
      val normPop = macroResult.simulatedPopulation%*%DenseMatrix.diagonal(macroResult.simulatedPopulation.colSum.map(1/_))
      innovationShares.zip(innovationUtilities).map{case (m,u)=> (normPop*m*(u/m.ncols)).sum}.sum
    }

    /**
      *
      * Diversity:
      *  - within cities? -> Herfindhal for each city
      *  - across cities? pb - given one innov does not sum to 1
      *  - across cities and innovations P_{i,c} = delta_{i,c} * P_i/P_tot is proba to find this innov in a random city
      *   H = 1 -  \sum_{i,c}{(delta_{i,c}* P_i/P_tot ) 2}
      *  - accross innov on total shares P_c = \sum_i P_i/P_tot delta_{i,c}
      *   H' = 1 - \sum_c P_c2
      *   (different from H)
      *
      *   Use H: try to be diverse both within and across cities
      *
      * @return
      */
    def averageDiversity: Double = {
      val normPop = macroResult.simulatedPopulation%*%DenseMatrix.diagonal(macroResult.simulatedPopulation.colSum.map(1/_))
      innovationShares.map(m => (normPop*m).map(x => x*x).colSum).reduceLeft(org.openmole.spatialdata.utils.math.+).map(1 - _).sum / normPop.ncols
    }

    /**
      * Number of innovation per city and unit of time
      * @return
      */
    def averageInnovation: Double = {
      innovationUtilities.length.toDouble/(macroResult.simulatedPopulation.nrows.toDouble*macroResult.simulatedPopulation.ncols.toDouble)
    }

    /**
     * gravity flows averaged in time and over cities
     * @return
     */
    def averageGravityFlow: Double = {
      gravityPotentials.map(m => m.sum / (m.ncols*m.nrows)).sum / gravityPotentials.size
    }

  }

  /**
    * Construct original model from setup files
    *
    * @param populationFile   pop file
    * @param distanceFile     dist file
    * @param datesFile        dates file
    * @param growthRate       g_0
    * @param innovationWeight w_I
    * @param gravityDecay     d_G
    * @param innovationDecay  d_I
    * @param innovationUtility       Utility of the first innovation
    * @param innovationUtilityGrowth Growth of the innovation utility (default to 1.12)
    * @param earlyAdoptersRate       Proportion of early adopters (defaults to 1%)
    * @param newInnovationHierarchy  City innovation hierarchy : exponent of the probability that a city introduces the new innovation (defaults to 1)
    * @param newInnovationPopulationProportion Proportion of population at which new innovation emerges (defaults to 0.5)
    * @return
    */
  def apply(populationFile: String,
            distanceFile: String,
            datesFile: String,
            seed: Int,
            growthRate: Double,
            innovationWeight: Double,
            gravityDecay: Double,
            innovationDecay: Double,
            innovationUtility: Double = 1.0,
            innovationUtilityGrowth: Double = 1.12,
            earlyAdoptersRate: Double = 0.01,
            newInnovationHierarchy: Double = 1.0,
            newInnovationPopulationProportion: Double = 0.5
           )(implicit m: MatrixImplementation = Matrix.defaultImplementation, rng: Random): Innovation = {
    rng.setSeed(seed.toLong)

    val populationMatrix = Matrix(CSV.readMat(populationFile))
    val distancesMatrix: Matrix = distanceFile.length match {
      case n if n > 0 => Matrix(CSV.readMat(distanceFile))
      case _ => DenseMatrix.zeros(populationMatrix.nrows,populationMatrix.ncols)
    }
    val dates = CSV.readCSV(datesFile,withHeader=false).values.toSeq.head.map(_.toDouble).toArray

    Innovation(populationMatrix,distancesMatrix,dates,rng,growthRate,innovationWeight,gravityDecay,innovationDecay,
      legacyInnovation(_,_,_,innovationUtilityGrowth,earlyAdoptersRate,newInnovationHierarchy,newInnovationPopulationProportion),
      innovationUtility
    )
  }

  /**
    * Synthetic setup for mutation model
    * @param syntheticCities number of cities
    * @param syntheticHierarchy initial hierarchy
    * @param syntheticMaxPop initial max population
    * @param finalTime number of time steps
    * @param seed seed
    * @param growthRate Gibrat growth rate
    * @param innovationWeight innovation growth rate
    * @param gravityDecay gravity decay
    * @param innovationDecay innovation diffusion decay
    * @param mutationRate mutation rate
    * @param newInnovationHierarchy new innovation hierarchy
    * @param earlyAdoptersRate early adoption proportion
    * @param utilityStd utility standard dviation
    * @param utilityDistribution type of distribution: "normal" or "log-normal"
    * @param rng rng
    * @return
    */
  def apply(
           syntheticCities: Int,
           syntheticHierarchy: Double,
           syntheticMaxPop: Double,
           finalTime: Int,
           seed: Int,
           growthRate: Double,
           innovationWeight: Double,
           gravityDecay: Double,
           innovationDecay: Double,
           mutationRate: Double,
           newInnovationHierarchy: Double,
           earlyAdoptersRate: Double,
           utilityStd: Double,
           utilityDistribution: String
           )(implicit rng: Random): Innovation = {
    implicit val m: MatrixImplementation = Matrix.defaultImplementation
    rng.setSeed(seed.toLong)

    val dmat = Matrix(Spatstat.euclidianDistanceMatrix(RandomPointsGenerator(syntheticCities).generatePoints.asPointSeq.toArray))
    val initialPopulations = Statistics.rankSizeDistribution(syntheticCities, syntheticHierarchy, syntheticMaxPop)
    val populationMatrix = DenseMatrix.zeros(syntheticCities,finalTime+1)
    populationMatrix.setMSubmat(0,0,Array(initialPopulations.toArray).transpose)
    val dates: Array[Double] = (0 to finalTime).toArray.map{_.toDouble}
    val distrib = utilityDistribution match {
      case "normal" => InnovationUtilityNormalDistribution()
      case "log-normal" =>
        assert(utilityStd>math.exp(-0.5),"For a log-normal distribution with mu=0, std must be > to 0.6")
        InnovationUtilityLogNormalDistribution()

    }

    Innovation(populationMatrix,dmat,dates,rng,growthRate,innovationWeight,gravityDecay,innovationDecay,
      mutationInnovation(_,_,_, mutationRate, newInnovationHierarchy, earlyAdoptersRate, utilityStd, distrib),
      1.0
    )

  }



  /**
    * Original model innovation process
    * @param population populations
    * @param utilities utility of innovations
    * @param innovationShares shares of innovations: innov[cities]
    * @param innovationUtilityGrowth fixed growth rate for utility
    * @param earlyAdoptersRate rate of early adoption
    * @param newInnovationHierarchy hierarchy to select a new innovative city
    * @param newInnovationPopulationProportion proportion needed for a new innovation to appear (deterministic)
    * @param rng rng
    * @return
    */
  def legacyInnovation(population: Seq[Double],
                       utilities: Seq[Double],
                       innovationShares: Seq[Seq[Double]],
                       innovationUtilityGrowth: Double,
                       earlyAdoptersRate: Double,
                       newInnovationHierarchy: Double,
                       newInnovationPopulationProportion: Double
                      )(implicit rng: Random): (Boolean, Seq[Double], Seq[Seq[Double]]) = {

    val latestInnovAdoption = innovationShares.last.zip(population).map{case (w,pop)=>w*pop}.sum / population.sum
    if (latestInnovAdoption > newInnovationPopulationProportion) {
      val newutility = utilities.last * innovationUtilityGrowth
      val (oldShares, newShares) = legacyNewInnovationShares(innovationShares.last,population, newInnovationHierarchy, earlyAdoptersRate)
      val newInnovationShares: Seq[Seq[Double]] = innovationShares.dropRight(1)++Seq(oldShares,newShares)
      (true, Seq(newutility), newInnovationShares)
    }else {
      (false, Seq.empty[Double], Seq.empty[Seq[Double]])
    }
  }


  /**
    * Returns new innovation and last innovation modified shares in the Favaro-Pumain model
    * @param previousInnovShares previous innovation shares across cities
    * @param currentPopulations current pops
    * @return
    */
  def legacyNewInnovationShares(previousInnovShares: Seq[Double],currentPopulations: Seq[Double], newInnovationHierarchy: Double, earlyAdoptersRate: Double)(implicit rng: Random): (Seq[Double], Seq[Double]) = {
    val innovativeCityIndex: Int = selectCityHierarchically(currentPopulations, newInnovationHierarchy)
    utils.log("Innovative city : "+innovativeCityIndex)
    val newShares: Seq[Double] = Seq.tabulate(previousInnovShares.length)(i => if(i==innovativeCityIndex) earlyAdoptersRate else 0.0)
    val oldShares: Seq[Double] = previousInnovShares.zipWithIndex.map{case (s,i) => if(i==innovativeCityIndex) s - earlyAdoptersRate else s}
    (oldShares, newShares)
  }


  /**
    * Innovation based on mutation
    * @param population population
    * @param utilities utilities
    * @param innovationShares innovation shares
    * @param mutationRate mutation rate
    * @param newInnovationHierarchy new innovation hierarchy
    * @param earlyAdoptersRate proportion of new innovation
    * @param utilityStd standard deviation of utility distribution
    * @param utilityDistribution type of distribution
    * @param rng rng
    * @return
    */
  def mutationInnovation(population: Seq[Double],
                         utilities: Seq[Double],
                         innovationShares: Seq[Seq[Double]],
                         mutationRate: Double,
                         newInnovationHierarchy: Double,
                         earlyAdoptersRate: Double,
                         utilityStd: Double,
                         utilityDistribution: InnovationUtilityDistribution
                        )(implicit rng: Random): (Boolean, Seq[Double], Seq[Seq[Double]]) = {
    def drawUtility: Double = utilityDistribution match {
      case _: InnovationUtilityNormalDistribution => utilities.sum/utilities.length + Stochastic.NormalDistribution(0.0,utilityStd).draw
      case _: InnovationUtilityLogNormalDistribution => utilities.sum/utilities.length - math.sqrt(utilityStd)*math.exp(0.25) + Stochastic.LogNormalDistribution(0.0,math.sqrt(math.log(utilityStd)+0.5)).draw
    }

    val pmax = population.max
    // max one innovation per city
    val (innovprobas,draws) = (population.map(p => mutationRate*math.pow(p/pmax,newInnovationHierarchy)),Seq.fill(population.length)(rng.nextDouble()))
    //utils.log("Innovation probas = "+innovprobas+" ; draws = "+draws)
    val innovativeCities: Seq[Int] = innovprobas.zip(draws).zipWithIndex.map{ case ((p,r),i) => if(r<p) Some(i) else None}.filter(_.isDefined).map(_.get)

    if (innovativeCities.nonEmpty) utils.log("Innovative cities: "+innovativeCities)

    if (innovativeCities.isEmpty) (false, Seq.empty[Double], Seq.empty[Seq[Double]])
    else {
      val newUtilities = innovativeCities.map(_ => math.max(0.1,drawUtility)) // min utility to avoid negative ones in case of large log-normal std
      utils.log("New utilities = "+newUtilities)
      // option: rescale or remove fixed proportion
      val newShares = innovationShares.map(_.zipWithIndex.map{case (s,i) => if(innovativeCities.contains(i)) s * (1 - earlyAdoptersRate) else s})++
        innovativeCities.map(i => Seq.tabulate(population.length)(j => if (j==i) earlyAdoptersRate else 0.0))
      //utils.log("New shares = "+newShares)
      (true, newUtilities, newShares)
    }
  }


  /**
    * Select a city hierarchically to population
    * @param currentPopulations current populations
    * @return
    */
  def selectCityHierarchically(currentPopulations: Seq[Double], newInnovationHierarchy: Double)(implicit rng: Random): Int = {
    val r = rng.nextDouble()
    val ptot = currentPopulations.map{math.pow(_,newInnovationHierarchy)}.sum
    Seq(Seq(0,currentPopulations.map{math.pow(_,newInnovationHierarchy)/ptot}.scanLeft(0.0)(_+_).indexWhere(_>r)).max,currentPopulations.length-1).min
  }


  def gravityPotentials(diffusedInnovs: Seq[Matrix], macroAdoptionLevels: Array[Double], currentPopulations: Array[Double], gravityDistanceWeights: Matrix, time: Int): Matrix = {
    val technoFactor: Array[Double] = diffusedInnovs.zip(macroAdoptionLevels).map{
      case(m,phi)=>
        m.getCol(time).flatValues.map{math.pow(_,phi)}
    }.toArray.foldLeft(Array.fill(diffusedInnovs.head.nrows)(1.0)){case(a1,a2)=>a1.zip(a2).map{case(d1,d2)=> d1*d2}}

    val totalpop = currentPopulations.sum
    val diagpops = DenseMatrix.diagonal(currentPopulations)*(1 / totalpop)
    diagpops %*% DenseMatrix.diagonal(technoFactor) %*% gravityDistanceWeights %*% diagpops
  }


  def nextState(model: Innovation, state: InnovationState): InnovationState = {

    utils.log(s"\n----Innovation step ${state.time}")

    import model._
    val gravityDistanceWeights = state.distanceMatrix.map { d => Math.exp(-d / gravityDecay) }
    val innovationDistanceWeights = distanceMatrix.map { d => Math.exp(-d / innovationDecay) }

    val n = populationMatrix.nrows
    val p = populationMatrix.ncols

    val delta_t = dates(state.time + 1) - dates(state.time)

    val totalpop = state.populations.sum

    val currentPopulations = state.populations.flatValues

    /*
      * 1) diffuse innovations
      */
    val tmplevel: Array[Array[Double]] = state.innovations.zip(state.utilities).map{
      case (m,utility)=>
        (Matrix(Array(
          m.getCol(state.time).flatValues.zip(currentPopulations).map{
            case(previousshare,citypop)=> math.pow(previousshare*citypop/totalpop,1/utility)}))(Matrix.defaultImplementation)%*%
          innovationDistanceWeights).flatValues
    }.toArray
    //utils.log("tmp level = "+tmplevel.map(_.toSeq).toSeq)
    val cumtmp: Array[Double] = tmplevel.foldLeft(Array.fill(n)(0.0)){case (a1,a2)=>a1.zip(a2).map{case(d1,d2)=>d1+d2}}
    val deltaci: Array[Array[Double]] = tmplevel.map{_.zip(cumtmp).map{case (d1,d2)=>d1 / d2}}
    //utils.log("deltaci = "+deltaci.map(_.toSeq).toSeq)
    val diffusedInnovs = state.innovations.zip(deltaci).map{
      case (innovmat,cityprops)=>
        val r = innovmat.clone
        //utils.log("     innov prev: "+r.getCol(state.time).flatValues.toSeq)
        r.setMSubmat(0,state.time + 1, cityprops.map(Array(_)))
        //utils.log("     innov diffused: "+r.getCol(state.time + 1).flatValues.toSeq)
        r
    }
    // compute macro adoption levels
    val macroAdoptionLevels: Array[Double] = diffusedInnovs.map{
      _.getCol(state.time + 1).flatValues.zip(currentPopulations).map{case(w,d)=>w*d}.sum
    }.map{_ / totalpop}.toArray
    utils.log("Macro levels = "+macroAdoptionLevels.toSeq)

    /*
      * 2) Update populations
      */
    val potsgravity = gravityPotentials(diffusedInnovs, macroAdoptionLevels, currentPopulations, gravityDistanceWeights, state.time + 1)
    val meanpotgravity = potsgravity.sum / (n * n)


    val prevpop = Matrix(currentPopulations.map(Array(_)))(Matrix.defaultImplementation)
    //res.setMSubmat(0, t,
     val newPopulations = (prevpop +
        (prevpop *
          (
            ((potsgravity %*% DenseMatrix.ones(n,1) * (innovationWeight / (n * meanpotgravity)))
              + DenseMatrix.constant(n, 1, growthRate)
              ) * delta_t
            )
          )
       ).flatValues
    //)
    //currentPopulations = res.getCol(t).flatValues
    utils.log(s"    Delta P = ${prevpop.flatValues.zip(newPopulations).map{case (p1,p2) => math.abs(p1 - p2)}.sum}")

    /*
      * 3) create a new innovation if needed
      */
    val currentInnovProps: Seq[Seq[Double]] = diffusedInnovs.map(_.getCol(state.time+1).flatValues.toSeq)
    val potentialInnovation: (Boolean, Seq[Double], Seq[Seq[Double]]) = model.newInnovation(newPopulations.toSeq, state.utilities.toSeq, currentInnovProps)
    val res = if (potentialInnovation._1){
      val newutilities = state.utilities ++ potentialInnovation._2
      val newShares = potentialInnovation._3
      // update old innovations (note that remaining of potentialInnovation._3 is ignored by the zip)
      val newInnovProp = state.innovations.toArray.zip(newShares).map{
        case (m,newprop) =>
          //utils.log("   update prop old = "+newprop)
          val r = m.clone
          r.setMSubmat(0,state.time + 1,Array(newprop.toArray).transpose)
          r
      }
      // add new innovation matrices
      val addInnovProp = newShares.takeRight(newShares.length-newInnovProp.length).map{s =>
        //utils.log("   update prop new = "+s.toSeq)
        val newInnovMat = DenseMatrix.zeros(n,p)
        newInnovMat.setMSubmat(0,state.time + 1,Array(s.toArray).transpose)
        newInnovMat
      }

      state.copy(
        time = state.time + 1,
        populations = Matrix(newPopulations, row = false)(Matrix.defaultImplementation),
        innovations = newInnovProp.toSeq++addInnovProp,
        utilities = newutilities,
        flows = potsgravity
      )

    } else state.copy(
      time = state.time + 1,
      populations = Matrix(newPopulations, row = false)(Matrix.defaultImplementation),
      innovations = diffusedInnovs,
      flows = potsgravity
    )

    //utils.log("  next innov state = "+res.innovations.map(_.values.map(_.toSeq).toSeq).mkString("\n"))

    res
  }


  /**
    * Run the model
    * @param model model
    * @return (MacroResult for population, seq of innovation utilities, seq of innovation proportions)
    */
  def run(model: Innovation): InnovationResult  = {

    utils.log("Running "+model.toString)

    import model._

    val n = populationMatrix.nrows
    val p = populationMatrix.ncols

    val res = DenseMatrix.zeros(n, p)
    res.setMSubmat(0, 0, populationMatrix.getCol(0).values)

    //var currentPopulations: Array[Double] = populationMatrix.getCol(0).values.flatten



    // the first innovation could already be in one city on top of a background archaic technology
    // (can be replaced by the second at the first step, consistent as assumed as coming from before the simulated period)
    // -> NO - better start with a single techno and let mutation process insert new
    val innovationUtilities: ArrayBuffer[Double] = new ArrayBuffer[Double]
    innovationUtilities.append(model.initialInnovationUtility)
    val innovationProportions: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]
    val archaicTechno =  DenseMatrix.zeros(n,p)
    archaicTechno.setMSubmat(0, 0, Array.fill(n)(Array(1.0)))
    innovationProportions.append(archaicTechno)

    val gravityPotentials: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]

    var currentState: InnovationState = InnovationState(time=0, populationMatrix.getCol(0), distanceMatrix, innovationProportions.toSeq, innovationUtilities.toSeq, flows = EmptyMatrix())

    for (t <- 1 until p) {

      utils.log(s"\n=================\nStep $t\n=================\n")

      currentState = Innovation.nextState(model, currentState)

      gravityPotentials.append(currentState.flows)
      //innovationUtilities.appendAll()
      res.setMSubmat(0, t, currentState.populations.values)

    }

    val currentPopulations = currentState.populations.flatValues
    val totalpop = currentPopulations.sum
    val macroAdoptionLevels: Array[Double] = currentState.innovations.map{
      _.getCol(p-1).flatValues.zip(currentPopulations).map{case(w,d)=>w*d}.sum
    }.map{_ / totalpop}.toArray

    utils.log("\n=================\n")
    utils.log("Innovations introduced : "+innovationProportions.length)
    utils.log("Macro adoption levels : "+macroAdoptionLevels.mkString(","))

    InnovationResult(MacroResultFit(model.populationMatrix,res),innovationUtilities.toSeq,innovationProportions.toSeq, gravityPotentials.toSeq)
  }



}
