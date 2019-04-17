
package org.openmole.spatialdata


package object grid {

  /**
    * String representation of a grid
    * @param world
    * @return
    */
  def gridToString(world: RasterLayerData[Double]): String = {
    world.map{_.map(_ match {case x if x > 0.0 => "+"; case x if x == 0 => "0"; case _ => "0"}).mkString("")}.mkString("\n")
  }

}