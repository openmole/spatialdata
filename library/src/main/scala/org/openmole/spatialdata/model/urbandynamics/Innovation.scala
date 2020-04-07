package org.openmole.spatialdata.model.urbandynamics


import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix}
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation

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
  * @param seed                    Random seed
  * @param growthRate              Gibrat growth rate
  * @param innovationWeight        weight of innovation induced growth rate
  * @param gravityDecay            Decay of gravity interaction
  * @param innovationDecay         Decay of innovation diffusion
  * @param innovationUtility       Utility of the first innovation
  * @param innovationUtilityGrowth Growth of the innovation utility (default to 1.12)
  * @param earlyAdoptersRate       Proportion of early adopters (defaults to 1%)
  * @param newInnovationHierarchy  City innovation hierarchy : exponent of the probability that a city introduces the new innovation (defaults to 1)
  * @param newInnovationPopulationProportion Proportion of population at which new innovation emerges (defaults to 0.5)
  */
case class Innovation(
                       populationMatrix: Matrix,
                       distanceMatrix: Matrix,
                       dates: Array[Double],
                       rng : Random,
                       seed : Int,
                       growthRate: Double,
                       innovationWeight: Double,
                       gravityDecay: Double,
                       innovationDecay: Double,
                       innovationUtility: Double,
                       innovationUtilityGrowth: Double,
                       earlyAdoptersRate: Double,
                       newInnovationHierarchy: Double,
                       newInnovationPopulationProportion: Double

                     ) extends MacroModel {

  override def run: MacroResult = Innovation.run(this)

  override def toString: String = "Innovation model with parameters"+
    "\n\tgrowthRate = "+growthRate+"\n\tinnovationWeight = "+innovationWeight+"\n\tgravityDecay = "+gravityDecay+
    "\n\tinnovationDecay = "+innovationDecay+"\n\tinnovationUtility = "+innovationUtility+"\n\tinnovationUtilityGrowth = "+innovationUtilityGrowth+
    "\n\tearlyAdoptersRate = "+earlyAdoptersRate+"\n\tnewInnovationHierarchy = "+newInnovationHierarchy+"\n\tnewInnovationPopulationProportion = "+newInnovationPopulationProportion

}


object Innovation {


  /**
    * Construct from setup files
    *
    * @param populationFile pop file
    * @param distanceFile dist file
    * @param datesFile dates file
    * @param growthRate g_0
    * @param innovationWeight w_I
    * @param gravityDecay d_G
    * @param innovationDecay d_I
    * @param innovationUtility U_I
    * @param innovationUtilityGrowth Delta U_I
    * @param earlyAdoptersRate r_0
    * @param newInnovationHierarchy alpha
    * @param newInnovationPopulationProportion p_0
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
    val populationMatrix = Matrix(CSV.readMat(populationFile))
    val distancesMatrix: Matrix = distanceFile.length match {
      case n if n > 0 => Matrix(CSV.readMat(distanceFile))
      case _ => DenseMatrix.zeros(populationMatrix.nrows,populationMatrix.ncols)
    }
    val dates = CSV.readCSV(datesFile,withHeader=false).values.toSeq(0).map(_.toDouble).toArray

    Innovation(populationMatrix,distancesMatrix,dates,rng,seed,growthRate,innovationWeight,gravityDecay,innovationDecay,innovationUtility,innovationUtilityGrowth,earlyAdoptersRate,newInnovationHierarchy,newInnovationPopulationProportion)
  }


  /**
    * Run the model
    * @param model model
    * @return
    */
  def run(model: Innovation): MacroResult = {

    utils.log("Running "+model.toString)

    import model._

    rng.setSeed(seed.toLong)

    val n = populationMatrix.nrows
    val p = populationMatrix.ncols

    /**
      * Select a city hierarchically to population
      * @param currentPopulations current populations
      * @return
      */
    def selectCityHierarchically(currentPopulations: Array[Double]): Int = {
      val r = rng.nextDouble
      val ptot = currentPopulations.map{math.pow(_,newInnovationHierarchy)}.sum
      Seq(Seq(0,currentPopulations.map{math.pow(_,newInnovationHierarchy)/ptot}.scanLeft(0.0)(_+_).indexWhere(_>r)).max,n-1).min
    }

    /**
      * Returns a Matrix for the new innov AND **Modifies in place the previous one**
      * @param previousInnovMatrix previous mat
      * @param currentPopulations current pops
      * @param time time step
      * @return
      */
    def newInnovationMatrix(previousInnovMatrix: Matrix,currentPopulations: Array[Double],time: Int): Matrix = {
      val innovativeCityIndex: Int = selectCityHierarchically(currentPopulations)
      utils.log("Innovative city : "+innovativeCityIndex)
      val diffrates: Matrix = DenseMatrix.zeros(n,p)
      diffrates.setM(innovativeCityIndex,time,earlyAdoptersRate)
      previousInnovMatrix.setM(innovativeCityIndex,time,previousInnovMatrix.get(innovativeCityIndex,time)-earlyAdoptersRate)
      diffrates
    }

    val inds = (0 until n by 1).toArray

    val res = DenseMatrix.zeros(n, p)
    res.setMSubmat(0, 0, populationMatrix.getCol(0).values)
    var currentPopulations: Array[Double] = populationMatrix.getCol(0).values.flatten

    val gravityDistanceWeights = distanceMatrix.map { d => Math.exp(-d / gravityDecay) }
    val innovationDistanceWeights = distanceMatrix.map { d => Math.exp(-d / innovationDecay) }

    val innovationUtilities: ArrayBuffer[Double] = new ArrayBuffer[Double]
    innovationUtilities.appendAll(Array(innovationUtility,innovationUtility*innovationUtilityGrowth))
    val innovationProportions: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]

    // the first innovation is already in one city on top of a background archaic technology
    // (can be replaced by the second at the first step, consistent as assumed as coming from before the simulated period)
    val archaicTechno =  DenseMatrix.zeros(n,p)
    archaicTechno.setMSubmat(0, 0, Array.fill(n)(Array(1.0)))
    innovationProportions.append(archaicTechno)
    innovationProportions.append(newInnovationMatrix(archaicTechno,currentPopulations,time=0))

    for (t <- 1 until p) {

      val delta_t = dates(t) - dates(t - 1)

      val totalpop = currentPopulations.sum

      /**
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


      /**
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

      /**
        * 3) create a new innovation if needed
        */
      val latestInnovAdoption = innovationProportions.last.getCol(t).flatValues.zip(currentPopulations).map{case (w,pop)=>w*pop}.sum / currentPopulations.sum
      if (latestInnovAdoption > newInnovationPopulationProportion) {
        val newutility = innovationUtilities.last * innovationUtilityGrowth
        innovationUtilities.append(newutility)
        innovationProportions.append(newInnovationMatrix(innovationProportions.last,currentPopulations,time = t))
      }

    }

    val totalpop = currentPopulations.sum
    val macroAdoptionLevels: Array[Double] = innovationProportions.map{
      _.getCol(p-1).flatValues.zip(currentPopulations).map{case(w,d)=>w*d}.sum
    }.map{_ / totalpop}.toArray

    utils.log("Innovations introduced : "+innovationProportions.length)
    utils.log("Macro adoption levels : "+macroAdoptionLevels.mkString(","))

    MacroResult(model.populationMatrix,res)
  }



}
