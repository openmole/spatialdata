
package spatialdata.synthetic.grid

import spatialdata._
import spatialdata.grid.GridGenerator

import scala.util.Random


case class ReactionDiffusionGridGenerator(
                                /**
                                  * The size of generated grids
                                  */
                                size : RasterDim,

                                /**
                                  * Population increase at each step
                                  */
                                growthRate : Int,

                                /**
                                  * total final population
                                  */
                                totalPopulation : Int,

                                /**
                                  * Strength of preferential attachment
                                  */
                                alpha : Double,

                                /**
                                  * Strength of diffusion
                                  */
                                beta : Double,

                                /**
                                  * Number of diffusions
                                  */
                                diffusionSteps : Int,

                                /**
                                  * Number of layers
                                  */
                                layers : Int = 1
                              ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] =
    ReactionDiffusionGridGenerator.reactionDiffusionGrid(size,growthRate,totalPopulation,alpha,beta,diffusionSteps,rng)
      .asInstanceOf[RasterLayerData[Double]]

}


object ReactionDiffusionGridGenerator {



  /**
    * Reaction diffusion grid generation
    * @param gridSize
    * @return
    */
  def reactionDiffusionGrid(size: RasterDim, growthRate: Double, totalPopulation: Double, alphaAtt: Double, diffusion: Double, diffusionSteps: Int, rng: Random): Array[Array[Double]] = {
    val (width,height)= size match {case Left(s)=>(s,s);case Right(c)=> c}

    var arrayVals = Array.fill(width, height) { 0.0 }
    var population: Double = 0

    while (population < totalPopulation) {

      // add new population following pref att rule
      if (population == 0) {
        //choose random patch
        for (_ ← 1 to growthRate.toInt) { val i = rng.nextInt(width); val j = rng.nextInt(height); arrayVals(i)(j) = arrayVals(i)(j) + 1 }
      }
      else {
        //val oldPop = arrayVals.map((a: Array[Double]) ⇒ a.map((c: Cell) ⇒ math.pow(c.population / population, alphaAtt)))
        val oldPop = arrayVals.map { _.map { case x ⇒ math.pow(x / population, alphaAtt) } }
        val ptot = oldPop.flatten.sum

        for (_ ← 1 to growthRate.toInt) {
          var s = 0.0; val r = rng.nextDouble(); var i = 0; var j = 0
          //draw the cell from cumulative distrib
          while (s < r) {
            s = s + (oldPop(i)(j) / ptot)
            j = j + 1
            if (j == height) { j = 0; i = i + 1 }
          }
          if (j == 0) { j = height - 1; i = i - 1 } else { j = j - 1 };
          arrayVals(i)(j) = arrayVals(i)(j) + 1
        }
      }

      // diffuse
      for (_ ← 1 to diffusionSteps) {
        arrayVals = diffuse(arrayVals, diffusion)
      }

      // update total population
      population = arrayVals.flatten.sum

    }
    //Seq.tabulate(size, size) { (i: Int, j: Int) ⇒ arrayVals(i)(j) }
    arrayVals
  }

  /**
    * Diffuse to neighbors proportion alpha of capacities
    *
    * // FIXME take into account square grids
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