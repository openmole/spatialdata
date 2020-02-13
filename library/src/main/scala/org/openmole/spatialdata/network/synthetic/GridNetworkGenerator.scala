
package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.network
import org.openmole.spatialdata.network._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * A grid network corresponds to links between neighbor cells of a grid
  * @param size
  * @param xstep
  * @param ystep
  * @param withDiagLinks
  */
case class GridNetworkGenerator(
                                 /**
                                   * Size of the (square) grid used to generate the network
                                   */
                                 size: Int,

                                 /**
                                   * x jump between two consecutive cells to connect
                                   */
                                 xstep: Int,

                                 /**
                                   * y jump between two consecutive cells to connect
                                   */
                                 ystep: Int,

                                 /**
                                   * include diagonal links (eight neighbors instead of four)
                                   */
                                 withDiagLinks: Boolean = false
                               ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = GridNetworkGenerator.gridNetwork(xstep=xstep,ystep=ystep,size=size,diagLinks = withDiagLinks)

}


object GridNetworkGenerator {



  /**
    *  by default, steps are at size / 10
    * @param size
    * @return
    */
  def apply(size: Int): GridNetworkGenerator = GridNetworkGenerator(size,size/10,size/10)

  /**
    * spatial grid network
    * @param xstep
    * @param ystep
    * @param size
    * @return
    */
  def gridNetwork(xstep: Int,ystep: Int, size: Int,diagLinks: Boolean = false): Network = {

    // create nodes
    val ycoords = (0 to size by ystep);val xcoords = (0 to size by xstep)
    val coords: Seq[(Double,Double)] = xcoords.map{case xx: Int => ycoords.map{case yy: Int => (xx.toDouble,yy.toDouble)}}.flatten
    val nodes: Seq[Seq[Node]] = coords.zipWithIndex.map{case c=>  Node(c._2,c._1._1,c._1._2)}.sliding(ycoords.size,ycoords.size).toSeq

    // create edges
    val edges = ArrayBuffer[Link]()
    //dirty
    for (i <- 0 to nodes.size - 1 ; j <- 0 to nodes(0).size - 1) {
      if(i-1>0){
        if(diagLinks&&j-1>0){edges.append(Link(nodes(i)(j),nodes(i-1)(j-1),0.0))}
        edges.append(Link(nodes(i)(j),nodes(i-1)(j),0.0))
        if(diagLinks&&j+1<nodes(0).size){edges.append(Link(nodes(i)(j),nodes(i-1)(j+1),0.0))}
      }
      if(j-1>0){
        edges.append(Link(nodes(i)(j),nodes(i)(j-1),0.0))
      }
      if(j+1<nodes(0).size){
        edges.append(Link(nodes(i)(j),nodes(i)(j+1),0.0))
      }
      if(i+1<nodes.size){
        if(diagLinks&&j-1>0){edges.append(Link(nodes(i)(j),nodes(i+1)(j-1),0.0))}
        edges.append(Link(nodes(i)(j),nodes(i+1)(j),0.0))
        if(diagLinks&&j+1<nodes(0).size){edges.append(Link(nodes(i)(j),nodes(i+1)(j+1),0.0))}
      }
    }

    network.Network(nodes.flatten.toSet,edges.toSet)
  }


}
