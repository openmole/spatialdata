package org.openmole.spatialdata.network



/**
  * Link of a spatial network
  * @param e1
  * @param e2
  * @param weight
  * @param length
  */
case class Link(e1: Node,e2: Node,weight: Double,length: Double, directed: Boolean) {

  /**
    * unique id if node ids are defined and unique
    * @return
    */
  def id: (Int,Int) = if(directed) (e1.id,e2.id) else {if(e1<=e2) (e1.id,e2.id) else (e2.id,e1.id)}

  /**
    * Link center x coordinate
    * @return
    */
  def x: Double = (e1.x + e2.x)/2

  /**
    * Link center y coordinate
    * @return
    */
  def y: Double = (e1.y + e2.y)/2

  /**
    * Direction of the link
    * @return
    */
  def heading: Double = (e1.x - e2.x,e1.y - e2.y) match {
    case (0.0,0.0) => 0.0 // by convention
    case (0.0,dy) => math.Pi/2*dy/math.abs(dy)
    case (dx,dy) => math.atan(dy/dx)
  }

  /**
    * Check if one x coordinate falls within the range of the link
    * @param xx
    * @return
    */
  def xwithin(xx: Double): Boolean = math.abs(x - xx) <= math.abs(length / 2 * math.cos(heading))
  def ywithin(yy: Double): Boolean = math.abs(y - yy) <= math.abs(length / 2 * math.sin(heading))

  /**
    * Compute intersection with an other link
    * @param l2
    * @return
    */
  def intersection(l2: Link): Option[Node] = {
    //println("inter? "+this+" ; "+l2)
    if(this.length==0.0||l2.length==0.0) return None
    if(e1==e2||l2.e1==l2.e2||e1==l2.e1||e1==l2.e2||e2==l2.e1||e2==l2.e2) return None // by convention no intersection if one common extremity
    val (h1,h2) = (heading,l2.heading)
    if(math.abs(h1)==math.Pi/2&&math.abs(h2)==math.Pi/2) return None
    val (m1,m2) = (math.tan(h1),math.tan(h2))
    if(m1 == m2) return None // parallel links
    if(math.abs(h1)==math.Pi/2){// this vertical link
    val c2 = l2.y - l2.x * m2
      val xx = this.x
      val yy = m2 * xx + c2
      if((!l2.xwithin(xx))||(!this.ywithin(yy))) return None else {
        //val inter = Node(0,digits(xx,4),digits(yy,4))
        val inter = Node(0,xx,yy)
        if (e1==inter||e2==inter||l2.e1==inter||l2.e2==inter) return None
        else return Some(inter)
      }
    }
    if(math.abs(h2)==math.Pi/2){ // vertical second link
      val c1 = this.y - this.x * m1
      val xx = l2.x
      val yy = m1 * xx + c1
      if((!xwithin(xx))||(!l2.ywithin(yy))) return None else {
        //val inter = Node(0,digits(xx,4),digits(yy,4))
        val inter = Node(0,xx,yy)
        if (e1==inter||e2==inter||l2.e1==inter||l2.e2==inter) return None
        else return Some(inter)
      }
    }
    // intercepts
    val c1 = this.y - this.x * m1
    val c2 = l2.y - l2.x * m2
    val xx = (c2 - c1) / (m1 - m2)
    if((!xwithin(xx))||(!l2.xwithin(xx))) None else {
      //val inter = Node(0,digits(xx,4),digits(m1 * xx + c1,4))
      val inter = Node(0,xx,m1 * xx + c1)
      if (e1==inter||e2==inter||l2.e1==inter||l2.e2==inter) return None
      else return Some(inter)
    }
  }

}

object Link {
  /**
    * Link with euclidian length
    * @param e1
    * @param e2
    * @param weight
    * @return
    */
  def apply(e1: Node,e2: Node,weight: Double, directed: Boolean): Link = {
    val d = math.sqrt((e1.x-e2.x)*(e1.x-e2.x)+(e1.y-e2.y)*(e1.y-e2.y))
    if (directed) Link(e1,e2,weight,d,directed) else {
      if (e1<=e2) Link(e1,e2,weight,d,directed) else Link(e2,e1,weight,d,directed)
    }
  }

  def apply(e1: Node,e2: Node,weight: Double): Link = Link(e1,e2,weight,false)

  def apply(e1: Node, e2: Node):Link = apply(e1,e2,1.0)

  def apply(e1: Node, e2: Node, directed: Boolean): Link = Link(e1,e2,1.0,directed)

  def getNodes(links: Set[Link]): Set[Node] = links.flatMap{l=>Set(l.e1,l.e2)}
}
