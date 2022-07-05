package org.openmole.spatialdata

import scala.collection.mutable.ArrayBuffer

import org.openmole.spatialdata.grid.RasterLayerData



/**
  * Network classes and functions
  *
  */
package object network {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  type ShortestPath = (Seq[Node],Seq[Link],Double)
  type ShortestPaths = Map[(Node,Node), (Seq[Node],Seq[Link],Double)]

  implicit class ShortestPathsDecorator(sp1: ShortestPaths){
    def |-|(sp2: ShortestPaths): Double =
      sp1.filter(c => (!c._2._3.isNaN)&(!c._2._3.isInfinity)).keySet.intersect(sp2.filter(c => (!c._2._3.isNaN)&(!c._2._3.isInfinity)).keySet).
        map{k => math.abs(sp1(k)._3 - sp2(k)._3)}.sum // path sets are assumed to be between same nodes
  }


  /*
  /**
    * a single network is a generator in itself
    * @param network network
    * @return
    */
  implicit def networkIsGenerator(network: Network): NetworkGenerator = new NetworkGenerator {
    //implicit val dummyRng = new Random
    override def generateNetwork(implicit rng: Random = new Random): Network = network
  }
  */



  /**
    * Network to grid: rasterize a network
    * @param network network
    * @return
    */
  def networkToGrid(network: Network,footPrintResolution: Double = 1.0,linkwidth: Double = 1.0): RasterLayerData[Double] = {
    val xmin = network.nodes.map{_.x}.min
    val xmax = network.nodes.map{_.x}.max
    val ymin = network.nodes.map{_.y}.min
    val ymax = network.nodes.map{_.y}.max
    def xcor(x: Double): Int = math.max(xmin,math.min(xmax,math.round(x).toDouble)).toInt
    def ycor(y: Double): Int = math.max(ymin,math.min(ymax,math.round(y))).toInt
    val res: Array[Array[Double]] = (BigDecimal(xmin) to xmax by 1.0).toArray.map{ _ => (BigDecimal(ymin) to ymax by 1.0).toArray.map{ _ =>0.0}}
    network.links.toSeq.filter{_.weight>0.0}.foreach{ l =>
      val i1 = l.e1.x - xmin;val j1 = l.e1.y - ymin
      val i2 = l.e2.x - xmin;val j2 = l.e2.y - ymin
      val istep = i1 - i2 match {case xx if math.abs(xx) < 1e-10 => 0.0 ;case _ => math.cos(math.atan((j2 - j1)/(i2 - i1)))*footPrintResolution}
      val jstep = j1 - j2 match {case xx if math.abs(xx) < 1e-10 => 0.0 ;case _ => math.sin(math.atan((j2 - j1)/(i2 - i1)))*footPrintResolution}
      val nsteps = i1 - i2 match {case xx if math.abs(xx) < 1e-10 => (j2 - j1)/jstep;case _ => (i2 - i1)/istep}
      var x = l.e1.x;var y = l.e1.y
      (BigDecimal(0.0) to nsteps by 1.0).foreach{_ =>
        for {
          k1 <- - BigDecimal((linkwidth-1)/2) to (linkwidth-1)/2 by 1.0
          k2 <-  - BigDecimal((linkwidth-1)/2) to (linkwidth-1)/2 by 1.0
        } yield {
          res(xcor(x+k1.toDouble))(ycor(y+k2.toDouble)) = 1.0
        }

        x = x + istep;y = y+ jstep
      }
    }
    res
  }


  /**
    * Reconstruct a network from the matrix representation of the world
    * (level of the patch, different from the generating network in the case of percolation)
    *  - put links in both senses world
    * @param world world
    * @return
    */
  def gridToNetwork(world: Array[Array[Double]]): Network = {
    val links = new ArrayBuffer[Link]()
    var nodeid = 0
    for(i <- world.indices; j <- world.indices) {
      if(world(i)(j)>0.0){
        val currentnode = Node(nodeid,i,j);nodeid=nodeid+1
        if(i-1>0){if(world(i-1)(j)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i-1,j)))}}
        if(i+1<world.length){if(world(i+1)(j)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i+1,j)))}}
        if(j-1>0){if(world(i)(j-1)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i,j-1)))}}
        if(j+1<world(0).length){if(world(i)(j+1)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i,j+1)))}}
      }
    }
    Network(links.map{_.e1}.toSet.union(links.map{_.e2}.toSet),links.toSet)
  }




  /**
    * simplify a spatial network through snapping
    * ! not implemented / put in graph algorithms or GIS?
    * @param network network
    * @param snap snapping threshold
    * @return
    */
  def spatialSimplification(network: Network,snap: Double): Network = {
    Network.empty
  }





}
