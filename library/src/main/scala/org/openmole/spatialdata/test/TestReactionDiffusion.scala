package org.openmole.spatialdata.test

import org.openmole.spatialdata.grid.synthetic.ReactionDiffusionGridGenerator
import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.measures.GridMorphology._
import org.openmole.spatialdata.utils._

import scala.util.Random


object TestReactionDiffusion {




  def benchmarkImplementation(): Unit ={

    implicit val rng = new Random

    val (grid,t) = withTimer[Unit,RasterLayerData[Double]]{_ => ReactionDiffusionGridGenerator(100,100,10000,2.0,0.05,1).generateGrid}()

    println(GridMorphology(grid,Seq(Moran(),AverageDistance(),Entropy(),Slope())))
    println(t)

  }


}
