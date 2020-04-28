package org.openmole.spatialdata.model.urbandynamics


import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix, Statistics, Stochastic}
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
  override def run: MacroResult = Innovation.run(this)._1






  override def toString: String = "Innovation model with parameters"+
    "\n\tgrowthRate = "+growthRate+"\n\tinnovationWeight = "+innovationWeight+"\n\tgravityDecay = "+gravityDecay+
    "\n\tinnovationDecay = "+innovationDecay

}


object Innovation {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

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
    val dates = CSV.readCSV(datesFile,withHeader=false).values.toSeq(0).map(_.toDouble).toArray

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
      case "log-normal" => InnovationUtilityLogNormalDistribution()
    }

    Innovation(populationMatrix,dmat,dates,rng,growthRate,innovationWeight,gravityDecay,innovationDecay,
      mutationInnovation(_,_,_, mutationRate, newInnovationHierarchy, earlyAdoptersRate, utilityStd, distrib),
      1.0
    )

  }

  sealed trait InnovationUtilityDistribution
  case class InnovationUtilityNormalDistribution() extends InnovationUtilityDistribution
  case class InnovationUtilityLogNormalDistribution() extends InnovationUtilityDistribution




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
    val innovativeCities: Seq[Int] = population.map(p => mutationRate*math.pow(p/pmax,newInnovationHierarchy)).
      zip(Seq.fill(population.length)(rng.nextDouble())).zipWithIndex.map{ case ((p,r),i) => if(p<r) Some(i) else None}.filter(_.isDefined).map(_.get)
    utils.log("Innovative cities: "+innovativeCities)

    if (innovativeCities.isEmpty) (false, Seq.empty[Double], Seq.empty[Seq[Double]])
    else {
      val newUtilities = innovativeCities.map(_ => drawUtility)
      // option: rescale or remove fixed proportion
      val newShares = innovationShares.map(_.zipWithIndex.map{case (s,i) => if(innovativeCities.contains(i)) s / (1 - earlyAdoptersRate) else s})++
        innovativeCities.map(i => Seq.tabulate(population.length)(j => if (j==i) earlyAdoptersRate else 0.0))
      (true, newUtilities, newShares)
    }
  }


  /**
    * Select a city hierarchically to population
    * @param currentPopulations current populations
    * @return
    */
  def selectCityHierarchically(currentPopulations: Seq[Double], newInnovationHierarchy: Double)(implicit rng: Random): Int = {
    val r = rng.nextDouble
    val ptot = currentPopulations.map{math.pow(_,newInnovationHierarchy)}.sum
    Seq(Seq(0,currentPopulations.map{math.pow(_,newInnovationHierarchy)/ptot}.scanLeft(0.0)(_+_).indexWhere(_>r)).max,currentPopulations.length-1).min
  }





  /**
    * Run the model
    * @param model model
    * @return (MacroResult for population, seq of innovation utilities, seq of innovation proportions)
    */
  def run(model: Innovation): (MacroResult, Seq[Double], Seq[Matrix])  = {

    utils.log("Running "+model.toString)

    import model._

    val n = populationMatrix.nrows
    val p = populationMatrix.ncols

    val res = DenseMatrix.zeros(n, p)
    res.setMSubmat(0, 0, populationMatrix.getCol(0).values)
    var currentPopulations: Array[Double] = populationMatrix.getCol(0).values.flatten

    val gravityDistanceWeights = distanceMatrix.map { d => Math.exp(-d / gravityDecay) }
    val innovationDistanceWeights = distanceMatrix.map { d => Math.exp(-d / innovationDecay) }

    // the first innovation could already be in one city on top of a background archaic technology
    // (can be replaced by the second at the first step, consistent as assumed as coming from before the simulated period)
    // -> NO - better start with a single techno and let mutation process insert new
    val innovationUtilities: ArrayBuffer[Double] = new ArrayBuffer[Double]
    innovationUtilities.append(model.initialInnovationUtility)
    val innovationProportions: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]
    val archaicTechno =  DenseMatrix.zeros(n,p)
    archaicTechno.setMSubmat(0, 0, Array.fill(n)(Array(1.0)))
    innovationProportions.append(archaicTechno)


    for (t <- 1 until p) {

      val delta_t = dates(t) - dates(t - 1)

      val totalpop = currentPopulations.sum

      /*
        * 1) diffuse innovations
        */
      val tmplevel: Array[Array[Double]] = innovationProportions.zip(innovationUtilities).map{
        case (m,u)=>
          (Matrix(Array(
          m.getCol(t-1).flatValues.zip(currentPopulations).map{
            case(w,d)=> math.pow(w*d,u)}))(Matrix.defaultImplementation)%*%innovationDistanceWeights).flatValues
      }.toArray
      val cumtmp: Array[Double] = tmplevel.foldLeft(Array.fill(n)(0.0)){case (a1,a2)=>a1.zip(a2).map{case(d1,d2)=>d1+d2}}
      val deltaci: Array[Array[Double]] = tmplevel.map{_.zip(cumtmp).map{case (d1,d2)=>d1 / d2}}
      innovationProportions.zip(deltaci).foreach{
        case (innovmat,cityprops)=>
          innovmat.setMSubmat(0,t, cityprops.map(Array(_)))
      }
      // compute macro adoption levels
      val macroAdoptionLevels: Array[Double] = innovationProportions.map{
        _.getCol(t).flatValues.zip(currentPopulations).map{case(w,d)=>w*d}.sum
      }.map{_ / totalpop}.toArray


      /*
        * 2) Update populations
        */
      val technoFactor: Array[Double] = innovationProportions.zip(macroAdoptionLevels).map{
        case(m,phi)=>
          m.getCol(t).flatValues.map{math.pow(_,phi)}
      }.toArray.foldLeft(Array.fill(n)(1.0)){case(a1,a2)=>a1.zip(a2).map{case(d1,d2)=> d1*d2}}

      val diagpops = DenseMatrix.diagonal(currentPopulations)*(1 / totalpop)
      val potsgravity = diagpops %*% DenseMatrix.diagonal(technoFactor) %*% gravityDistanceWeights %*% diagpops
      val meanpotgravity = potsgravity.sum / (n * n)

      val prevpop = Matrix(currentPopulations.map(Array(_)))(Matrix.defaultImplementation)
      res.setMSubmat(0, t,
        (prevpop +
          (prevpop *
            (
              ((potsgravity %*% DenseMatrix.ones(n,1) * (innovationWeight / (n * meanpotgravity)))
                + DenseMatrix.constant(n, 1, growthRate)
               ) * delta_t
            )
           )
         ).values
      )
      currentPopulations = res.getCol(t).flatValues

      /*
        * 3) create a new innovation if needed
        */
      val currentInnovProps: Seq[Seq[Double]] = innovationProportions.map(_.getCol(t).flatValues.toSeq).toSeq
      val potentialInnovation: (Boolean, Seq[Double], Seq[Seq[Double]]) = model.newInnovation(currentPopulations.toSeq, innovationUtilities.toSeq, currentInnovProps)
      if (potentialInnovation._1){
        innovationUtilities.appendAll(potentialInnovation._2)
        val newShares = potentialInnovation._3
        // update old innovations (note that remaining of potentialInnovation._3 is ignored by the zip)
        innovationProportions.toArray.zip(newShares).foreach{case (m,newprop) => m.setMSubmat(0,t,Array(newprop.toArray).transpose)}
        // add new innovation matrices
        newShares.takeRight(newShares.length-innovationProportions.length).foreach{s =>
          val newInnovMat = DenseMatrix.zeros(n,p)
          newInnovMat.setMSubmat(0,t,Array(s.toArray).transpose)
          innovationProportions.append(newInnovMat)
        }
      }

    }

    val totalpop = currentPopulations.sum
    val macroAdoptionLevels: Array[Double] = innovationProportions.map{
      _.getCol(p-1).flatValues.zip(currentPopulations).map{case(w,d)=>w*d}.sum
    }.map{_ / totalpop}.toArray

    utils.log("Innovations introduced : "+innovationProportions.length)
    utils.log("Macro adoption levels : "+macroAdoptionLevels.mkString(","))

    (MacroResult(model.populationMatrix,res),innovationUtilities.toSeq,innovationProportions.toSeq)
  }



}
