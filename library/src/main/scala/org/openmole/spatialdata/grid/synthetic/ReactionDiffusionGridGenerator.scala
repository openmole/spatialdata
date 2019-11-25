
package org.openmole.spatialdata.grid.synthetic

import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.GridGenerator

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
                                initialConfiguration: Option[RasterLayerData[Double]] = None,
                                layers : Int = 1
                              ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] =
    ReactionDiffusionGridGenerator.reactionDiffusionGrid(size,growthRate,totalPopulation,alpha,beta,diffusionSteps)

}


object ReactionDiffusionGridGenerator {



  /**
    * Reaction diffusion grid generation
    * @param gridSize
    * @return
    */
  def reactionDiffusionGrid(size: RasterDim, growthRate: Double, totalPopulation: Double, alphaAtt: Double, diffusion: Double, diffusionSteps: Int, initialConfiguration: Option[RasterLayerData[Double]] = None,iterImpl: Boolean = false)(implicit rng: Random): Array[Array[Double]] = {
    val (width,height)= size match {case Left(s)=>(s,s);case Right(c)=> c}

    var arrayVals = initialConfiguration.getOrElse(Array.fill(width, height) { 0.0 })
    var population: Double = arrayVals.flatten.sum

    while (population < totalPopulation) {

      // add new population following pref att rule
      if (population == 0) {
        //choose random patch
        for (_ ← 1 to growthRate.toInt) { val i = rng.nextInt(width); val j = rng.nextInt(height); arrayVals(i)(j) = arrayVals(i)(j) + 1 }
      }
      else {
        val oldPop: Array[Array[Double]] = arrayVals.map { _.map { case x ⇒ math.pow(x / population, alphaAtt) } }
        val ptot = oldPop.flatten.sum

        if (iterImpl) {
          for (_ ← 1 to growthRate.toInt) {
            var s = 0.0; val r = rng.nextDouble(); var i = 0; var j = 0
            //draw the cell from cumulative distrib
            while (s < r) {
              s = s + (oldPop(i)(j) / ptot)
              j = j + 1
              if (j == height) {
                j = 0; i = i + 1
              }
            }
            if (j == 0) {
              j = height - 1; i = i - 1
            } else {
              j = j - 1
            };
            arrayVals(i)(j) = arrayVals(i)(j) + 1
          }
        }else {

          val probas = Array.fill(growthRate.toInt)(rng.nextDouble())
          val flatpops = oldPop.zipWithIndex.flatMap { case (r, i) => r.zipWithIndex.map {(_, i)}}
          //println(probas.toSeq)
          //println(flatpops)

          //oldPop.map(_.zipWithIndex).zipWithIndex.flatMap{case ((p,j),i) => }
          // no need to stack drawn states (side effect in the population array)
          def nextProba(state: (Array[Double], Array[((Double, Int), Int)], Double, (Double, Int, Int))): (Array[Double], Array[((Double, Int), Int)], Double, (Double, Int, Int)) = {
            println(state)
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

          println("prev pop = "+arrayVals.flatten.sum)
          Iterator.iterate((probas,flatpops , 0.0, (0.0, 0, 0)))(nextProba).takeWhile(!_._1.isEmpty)
          println("it pop = "+arrayVals.flatten.sum)
        }

      }

      // diffuse
      for (_ ← 1 to diffusionSteps) {
        arrayVals = diffuse(arrayVals, diffusion)
      }

      // update total population
      population = arrayVals.flatten.sum

    }
    arrayVals
  }

  /**
    * Diffuse to neighbors proportion alpha of capacities
    *
    *  FIXME take into account NON square grids
    *  FIXME can be done with convolution
    *
    * @param a
    */
  def diffuse(a: Array[Array[Double]], alpha: Double): Array[Array[Double]] = {
    val newVals = a.clone()
    val size = a.length

    for (i ← 0 to size - 1; j ← 0 to size - 1) {
      // diffuse in neigh cells
      if (i >= 1) { newVals(i - 1)(j) = newVals(i - 1)(j) + (alpha / 8) * a(i)(j) }
      if (i < size - 1) { newVals(i + 1)(j) = newVals(i + 1)(j) + (alpha / 8) * a(i)(j) }
      if (j >= 1) { newVals(i)(j - 1) = newVals(i)(j - 1) + (alpha / 8) * a(i)(j) }
      if (j < size - 1) { newVals(i)(j + 1) = newVals(i)(j + 1) + (alpha / 8) * a(i)(j) }
      if (i >= 1 && j >= 1) { newVals(i - 1)(j - 1) = newVals(i - 1)(j - 1) + (alpha / 8) * a(i)(j) }
      if (i >= 1 && j < size - 1) { newVals(i - 1)(j + 1) = newVals(i - 1)(j + 1) + (alpha / 8) * a(i)(j) }
      if (i < size - 1 && j >= 1) { newVals(i + 1)(j - 1) = newVals(i + 1)(j - 1) + (alpha / 8) * a(i)(j) }
      if (i < size - 1 && j < size - 1) { newVals(i + 1)(j + 1) = newVals(i + 1)(j + 1) + (alpha / 8) * a(i)(j) }
      //delete in the cell (¡ bord effect : lost portion is the same even for bord cells !)
      // to implement diffuse as in NL, put deletion inside boundary conditions checking
      newVals(i)(j) = newVals(i)(j) - alpha * a(i)(j)
    }
    newVals
  }



}