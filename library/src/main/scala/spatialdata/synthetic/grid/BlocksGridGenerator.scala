
package spatialdata.synthetic.grid

import spatialdata.{RasterDim, RasterLayerData}
import spatialdata.grid.GridGenerator

import scala.util.Random


case class BlocksGridGenerator(
                                /**
                                  * size of the grid
                                  */
                              size: RasterDim,

                                /**
                                  * number of blocks randomly added
                                  */
                              blocks: Int,

                                /**
                                  * minimal width/height of blocks
                                  */
                              blockMinSize: Int,


                                /**
                                  * maximal width/height of blocks
                                  */
                                blockMaxSize: Int

                              ) extends GridGenerator {

  /**
    *
    * @param rng
    * @return
    */
  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = BlocksGridGenerator.blocksGrid(size,blocks,blockMinSize,blockMaxSize,rng)

}



object BlocksGridGenerator {


  /**
    *
    * @param size
    * @return
    */
  def blocksGrid(size: RasterDim,blocks: Int,blockMinSize: Int, blockMaxSize: Int,rng: Random): RasterLayerData[Double] = {
    println("Blocks grid of size "+size+" ; "+blocks+" ; "+blockMinSize+" ; "+blockMaxSize)
    val maxsize = math.max(blockMinSize,blockMaxSize)
    val minsize = math.min(blockMinSize,blockMaxSize)
    val w = size match {case Left(l) => l; case Right((w,_)) => w}
    val h = size match {case Left(l) => l; case Right((_,h)) => h}
    val vals = Array.fill(w,h)(0.0)
    for(_ <- 0 to blocks - 1){
      val (i,j) = (rng.nextInt(w),rng.nextInt(h))
      val (ww,hh) = (minsize + rng.nextInt(maxsize-minsize + 1),minsize + rng.nextInt(maxsize-minsize + 1))
      // convention : if even, center on bottom right corner
      for(di <- 0 to ww - 1 ){
        for(dj <- 0 to hh - 1){
          val (k,l) = (i - ww / 2 + di,j - hh / 2 + dj)
          if(k>=0&l>=0&k<w&l<h){vals(k)(l) = vals(k)(l) + 1.0}
        }
      }

    }
    vals
  }


}





