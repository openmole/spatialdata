
package org.openmole.spatialdata.grid.synthetic

import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.grid.GridGenerator

import scala.util.Random

/**
  *
  *  - implement additional rotation ? -> pb as adds additional parameters
  *
  * @param size size of the grid
  * @param blocks number of blocks randomly added
  * @param blockMinSize minimal width/height of blocks
  * @param blockMaxSize maximal width/height of blocks
  * @param rotationMax maximal value of random rotation
  */
case class BlocksGridGenerator(
                              size: RasterDim,
                              blocks: Int,
                              blockMinSize: Int,
                              blockMaxSize: Int,
                              rotationMax: Double = 0.0
                              ) extends GridGenerator {

  /**
    *
    * @param rng rng
    * @return
    */
  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = BlocksGridGenerator.blocksGrid(size,blocks,blockMinSize,blockMaxSize,rng)

}



object BlocksGridGenerator {


  /**
    *
    * @param size size
    * @return
    */
  def blocksGrid(size: RasterDim,blocks: Int,blockMinSize: Int, blockMaxSize: Int,rng: Random): RasterLayerData[Double] = {
    //println("Blocks grid of size "+size+" ; "+blocks+" ; "+blockMinSize+" ; "+blockMaxSize)
    val maxsize = math.max(blockMinSize,blockMaxSize)
    val minsize = math.min(blockMinSize,blockMaxSize)
    val w = size match {case Left(l) => l; case Right((ww,_)) => ww}
    val h = size match {case Left(l) => l; case Right((_,hh)) => hh}
    val vals = Array.fill(w,h)(0.0)
    for(_ <- 0 until blocks){
      val (i,j) = (rng.nextInt(w),rng.nextInt(h))
      val (ww,hh) = (minsize + rng.nextInt(maxsize-minsize + 1),minsize + rng.nextInt(maxsize-minsize + 1))
      // convention : if even, center on bottom right corner
      for(di <- 0 until ww){
        for(dj <- 0 until hh){
          val (k,l) = (i - ww / 2 + di,j - hh / 2 + dj)
          if(k>=0&l>=0&k<w&l<h){vals(k)(l) = vals(k)(l) + 1.0}
        }
      }

    }
    vals
  }


}





