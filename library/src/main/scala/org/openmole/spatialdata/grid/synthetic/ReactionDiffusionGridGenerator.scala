
package org.openmole.spatialdata.grid.synthetic

import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.grid.GridGenerator

import scala.runtime.RichDouble
import scala.util.Random


/**
  *
  * @param size The size of generated grids
  * @param growthRate Population increase at each step
  * @param totalPopulation total final population
  * @param alpha Strength of preferential attachment
  * @param beta Strength of diffusion
  * @param diffusionSteps Number of diffusions
  * @param initialConfiguration optional initial config
  * @param layers Number of layers
  */
case class ReactionDiffusionGridGenerator(
                                size : RasterDim,
                                growthRate : Int,
                                totalPopulation : Int,
                                alpha : Double,
                                beta : Double,
                                diffusionSteps : Int,
                                initialConfiguration: Option[Seq[Seq[Double]]] = None,
                                layers : Int = 1,
                                iterImpl: Boolean = false
                              ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] =
    ReactionDiffusionGridGenerator.reactionDiffusionGrid(size,growthRate,totalPopulation,alpha,beta,diffusionSteps,initialConfiguration,iterImpl=iterImpl)

}


object ReactionDiffusionGridGenerator {


  /**
    * Reaction diffusion grid generation
    * @param size grid size
    * @param growthRate growth rate N_G
    * @param totalPopulation total population P_m
    * @param alphaAtt strength of preferential attachment
    * @param diffusion strength of diffusion
    * @param diffusionSteps number of diffusion steps
    * @param initialConfiguration optional initial configuration
    * @param iterImpl use the iterative implementation
    * @param rng rng
    * @return
    */
  def reactionDiffusionGrid(size: RasterDim, growthRate: Double, totalPopulation: Double, alphaAtt: Double, diffusion: Double, diffusionSteps: Int, initialConfiguration: Option[Seq[Seq[Double]]] = None,iterImpl: Boolean = true)(implicit rng: Random): Array[Array[Double]] = {
    // FIXME inversion height - width
    val (width,height)= if(initialConfiguration.isDefined) (initialConfiguration.get.length,initialConfiguration.get(0).length) else size match {case Left(s)=>(s,s);case Right(c)=> c}

    var arrayVals: Array[Array[Double]] = if (initialConfiguration.isDefined) {
      val copyconfig = initialConfiguration.get.map{_.toVector}.toVector
      Array.tabulate(width,height){ (i,j) => copyconfig(i)(j)}
    } else Array.fill(width, height) { 0.0 }
    var population: Double = arrayVals.flatten.filter(!_.isNaN).sum

    val deltapop = totalPopulation - population

    var steps = 0
    var stepdeltapop = 0.0

    //log("ReacDiff Ng = "+growthRate.toInt)
    //assert(growthRate.toInt>0,"no meso growth : tot pop = "+arrayVals.flatten.sum)
    // growthRates < 1 will lead to no increment : either discretize in smaller units or keep this small discrepency ? (if really pop counts should be reasonable)


    // FIXME thematic Q : are negative pop increment just a bias in the heuristic or could be interpreted realistically? NO because get out only from boundaries: true if more sprawl, no meaning if external migration
    while (population < totalPopulation && stepdeltapop>=0.0) {

      val prevpop = population

      // add new population following pref att rule
      if (population == 0) {
        //choose random patch - if Nan a few will be lost
        for (_ <- 1 to growthRate.toInt) { val i = rng.nextInt(width); val j = rng.nextInt(height); arrayVals(i)(j) = arrayVals(i)(j) + 1 }
      }
      else {

        val oldPop: Array[Array[Double]] = arrayVals.map { _.map { case x => math.pow(x / population, alphaAtt) } }
        val ptot = oldPop.flatten.filter(!_.isNaN).sum

        if (iterImpl) {
          for (_ <- 1 to growthRate.toInt) {
            var s = 0.0; val r = rng.nextDouble(); var i = 0; var j = 0
            //draw the cell from cumulative distrib
            while (s < r) {
              val d = oldPop(i)(j)
              if (!d.isNaN) s = s + (d / ptot)
              j = j + 1
              if (j == height) {
                j = 0; i = i + 1
              }
            }
            if (j == 0) {
              j = height - 1; i = i - 1
            } else {
              j = j - 1
            }
            arrayVals(i)(j) = arrayVals(i)(j) + 1
          }
        }else {

          val probas = Array.fill(growthRate.toInt)(rng.nextDouble()).sorted(Ordering.Double.TotalOrdering)

          //println(probas.toSeq)

          var s = 0.0
          var i = 0
          var j = 0
          var k = 0
          //draw the cell from cumulative distrib
          while (k < probas.length) {
            if (s <= probas(k)) {
              val d = oldPop(i)(j)
              if (!d.isNaN) s = s + (d / ptot)
              j = j + 1
              if (j == height) {
                j = 0
                i = i + 1
              }
            } else {
              k = k + 1
              val (ii,jj) = if (j == 0) (i - 1,height - 1) else (i,j - 1)

              arrayVals(ii)(jj) = arrayVals(ii)(jj) + 1
            }
        }

          /*
          // functional recursive implementation is WAY slower (10 times) -> something not done right ? ! forgot to sort probas
          // still 4times slower

          val flatpops = oldPop.zipWithIndex.flatMap { case (r, i) => r.zipWithIndex.map {(_, i)}}
          //println(probas.toSeq)
          //println(flatpops)

          //oldPop.map(_.zipWithIndex).zipWithIndex.flatMap{case ((p,j),i) => }
          // no need to stack drawn states (side effect in the population array)
          def nextProba(state: (Array[Double], Array[((Double, Int), Int)], Double, (Double, Int, Int))): (Array[Double], Array[((Double, Int), Int)], Double, (Double, Int, Int)) = {
            //println(state)
            if (state._1.isEmpty) return (Array.empty, Array.empty, 0.0, (0.0, 0, 0))
            // as probas sum to one, second array will never be empty
            if (state._1.head <= state._3) {
              // dirty to use side effect like this, pop array should be in the state - but better for memory ?
              arrayVals(state._4._2)(state._4._3) = arrayVals(state._4._2)(state._4._3) + 1
              nextProba(state.copy(_1 = state._1.tail))
            } else {
              val ((d, j), i) = state._2.head
              nextProba(state.copy(_2 = state._2.tail, _3 = state._3 + d / ptot, _4 = (d, i, j)))
            }
          }

          val initstate = (probas,flatpops , 0.0, (0.0, 0, 0))
          //println("prev pop = "+arrayVals.flatten.sum)
          val res = nextProba(initstate)
          //println("it pop = "+arrayVals.flatten.sum)

          //Iterator.iterate(initstate)(nextProba).takeWhile(!_._1.isEmpty).toSeq
          */


        }

      }

      // diffuse
      val prediffpop = arrayVals.flatten.filter(!_.isNaN).sum
      for (_ <- 1 to diffusionSteps) {
        arrayVals = diffuse(arrayVals, diffusion)
      }

      // update total population
      population = arrayVals.flatten.filter(!_.isNaN).sum

      stepdeltapop = population - prevpop
      steps = steps + 1
      if (steps%1000==0) println(s"$steps: Prop ${population / totalPopulation} ;Increase = ${(prediffpop - prevpop)/deltapop} ; diff ${(prediffpop-population)/deltapop}")
      //println("NaNs % = "+arrayVals.flatten.filter(_.isNaN).length.toDouble / arrayVals.flatten.length.toDouble)


    }
    arrayVals
  }

  /**
    * Diffuse to neighbors proportion alpha of capacities
    *
    *  FIXME can be done with convolution
    *
    * @param a matrix to diffuse
    * @param alpha strength of diffusion
    */
  def diffuse(a: Array[Array[Double]], alpha: Double): Array[Array[Double]] = {
    val newVals = a.clone()
    val (height,width) = (a.length,a(0).length)

    for (i <- a.indices; j <- a(0).indices) {
      // diffuse in neigh cells
      val d = a(i)(j)
      if (!d.isNaN) {
        if (i >= 1) {
          newVals(i - 1)(j) = newVals(i - 1)(j) + (alpha / 8) * d
        }
        if (i < height - 1) {
          newVals(i + 1)(j) = newVals(i + 1)(j) + (alpha / 8) * d
        }
        if (j >= 1) {
          newVals(i)(j - 1) = newVals(i)(j - 1) + (alpha / 8) * d
        }
        if (j < width - 1) {
          newVals(i)(j + 1) = newVals(i)(j + 1) + (alpha / 8) * d
        }
        if (i >= 1 && j >= 1) {
          newVals(i - 1)(j - 1) = newVals(i - 1)(j - 1) + (alpha / 8) * d
        }
        if (i >= 1 && j < width - 1) {
          newVals(i - 1)(j + 1) = newVals(i - 1)(j + 1) + (alpha / 8) * d
        }
        if (i < height - 1 && j >= 1) {
          newVals(i + 1)(j - 1) = newVals(i + 1)(j - 1) + (alpha / 8) * d
        }
        if (i < height - 1 && j < width - 1) {
          newVals(i + 1)(j + 1) = newVals(i + 1)(j + 1) + (alpha / 8) * d
        }
        //delete in the cell (¡ bord effect : lost portion is the same even for bord cells !)
        // to implement diffuse as in NL, put deletion inside boundary conditions checking
        newVals(i)(j) = newVals(i)(j) - alpha * d
      }
    }
    newVals
  }



}