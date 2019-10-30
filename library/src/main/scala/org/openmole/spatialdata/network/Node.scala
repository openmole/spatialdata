package org.openmole.spatialdata.network

import org.openmole.spatialdata.Point2D



/**
  * Node of a spatial network
  * @param id // note: how to impose comparison only on positions? -> all ids to 0?
  * @param position point in a 2D space
  */
case class Node(id: Int, position: Point2D) {
  def x: Double = position._1
  def y: Double = position._2

  def distance(n2: Node): Double = math.sqrt(math.pow(x-n2.x,2)+math.pow(y-n2.y,2))

  /**
    * lexicographic order
    * @param n2
    * @return
    */
  def <=(n2: Node): Boolean = x<n2.x||(x==n2.x&&y<=n2.y)

}

object Node {
  def apply(id: Int,x: Double,y: Double): Node = Node(id,(x,y))
}


