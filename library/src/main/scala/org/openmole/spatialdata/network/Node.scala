package org.openmole.spatialdata.network

import org.openmole.spatialdata.vector.{Attributes, Point}



/**
  * Node of a spatial network
  * @param id // note: how to impose comparison only on positions? -> all ids to 0?
  * @param position point in a 2D space
  */
case class Node(
                 id: Int,
                 position: Point,
                 attributes: Attributes = Attributes.empty
               ) {
  def x: Double = position._1
  def y: Double = position._2

  //def asPoint: Point = (x,y) // just use position

  def distance(n2: Node): Double = math.sqrt(math.pow(x-n2.x,2)+math.pow(y-n2.y,2))

  /**
    * lexicographic order
    * @param n2 other node
    * @return
    */
  def <=(n2: Node): Boolean = x<n2.x||(x==n2.x&&y<=n2.y)

  /**
    * Find the projection of a node on a set of links
    * @param links links
    * @return
    */
  def projection(links: Set[Link]): (Node,Link) = {
    val (pmin,lmin,_) = links.map{l => {
        val p = projection(l)
        (p,l,distance(p))
      }
    }.minBy(_._3)
    (pmin,lmin)
  }

  /**
    * projection on one link
    * @param link link
    * @return
    */
  def projection(link: Link): Node = {
    val (x1,y1,x2,y2) = (link.e1.x,link.e1.y,link.e2.x,link.e2.y)
    val m1m = math.sqrt(math.pow(x1 - x,2) + math.pow(y1 - y,2))
    val m2m = math.sqrt(math.pow(x2 - x,2) + math.pow(y2 - y, 2))
    val m1m2 = math.sqrt(math.pow(x1 - x2, 2) + math.pow(y1 - y2, 2))
    if (m1m == 0||m1m2 == 0) return link.e1
    if (m2m == 0) return link.e2
    val cost1 = (((x - x1)*(x2 - x1)) + ((y - y1)*(y2 - y1)))/(m1m * m1m2)
    val cost2 = (((x - x2)*(x1 - x2)) + ((y - y2)*(y1 - y2)))/(m2m * m1m2)
    if (cost1 < 0) link.e1
    else {
      if (cost2 < 0) link.e2
      else {
        val mq = m1m * math.sqrt(math.abs(1 - math.pow(cost1,2)))
        val m1q = math.sqrt(math.pow(m1m,2) - math.pow(mq,2))
        Node(0,x1 + m1q * (x2 - x1) / m1m2 ,y1 + m1q * (y2 - y1) / m1m2)
      }
    }
  }
}

object Node {
  def apply(id: Int,x: Double,y: Double): Node = Node(id,(x,y))

  def apply(p: Point): Node = Node(0,p)

  val empty: Node = Node(-1,0.0,0.0)
}


