
package org.openmole.spatialdata.network.synthetic

import org.openmole.spatialdata.network
import org.openmole.spatialdata.network._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * A grid network corresponds to links between neighbor cells of a grid
  * @param size  Size of the (square) grid used to generate the network
  * @param xstep x jump between two consecutive cells to connect
  * @param ystep y jump between two consecutive cells to connect
  * @param withDiagLinks include diagonal links (eight neighbors instead of four)
  */
case class GridNetworkGenerator(
                                 size: Int,
                                 xstep: Double,
                                 ystep: Double,
                                 withDiagLinks: Boolean = false
                               ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = GridNetworkGenerator.gridNetwork(xstep=xstep,ystep=ystep,size=size,diagLinks = withDiagLinks)

}


object GridNetworkGenerator {



  /**
    *  by default, steps are at size / 10
    * @param size size
    * @return
    */
  def apply(size: Int): GridNetworkGenerator = GridNetworkGenerator(size,size.toDouble/10.0,size.toDouble/10.0)

  /**
    * spatial grid network
    * @param xstep x step
    * @param ystep y step
    * @param size size
    * @return
    */
  def gridNetwork(xstep: Double,ystep: Double, size: Int,diagLinks: Boolean = false): Network = {

    // create nodes
    val ycoords = (0 to (size/ystep).toInt by 1).map(_*ystep)
    val xcoords = (0 to (size/xstep).toInt by 1).map(_*xstep)
    val coords: Seq[(Double,Double)] = xcoords.flatMap{x => ycoords.map{y => (x,y)}}
    val nodes: Seq[Seq[Node]] = coords.zipWithIndex.map{c=>  Node(c._2,c._1._1,c._1._2)}.sliding(ycoords.size,ycoords.size).toSeq

    // create edges
    val edges = ArrayBuffer[Link]()
    //dirty
    for (i <- nodes.indices ; j <- nodes.indices) {
      if(i-1>0){
        if(diagLinks&&j-1>0){edges.append(Link(nodes(i)(j),nodes(i-1)(j-1),0.0))}
        edges.append(Link(nodes(i)(j),nodes(i-1)(j),0.0))
        if(diagLinks&&j+1<nodes.head.size){edges.append(Link(nodes(i)(j),nodes(i-1)(j+1),0.0))}
      }
      if(j-1>0){
        edges.append(Link(nodes(i)(j),nodes(i)(j-1),0.0))
      }
      if(j+1<nodes.head.size){
        edges.append(Link(nodes(i)(j),nodes(i)(j+1),0.0))
      }
      if(i+1<nodes.size){
        if(diagLinks&&j-1>0){edges.append(Link(nodes(i)(j),nodes(i+1)(j-1),0.0))}
        edges.append(Link(nodes(i)(j),nodes(i+1)(j),0.0))
        if(diagLinks&&j+1<nodes.head.size){edges.append(Link(nodes(i)(j),nodes(i+1)(j+1),0.0))}
      }
    }

    network.Network(nodes.flatten.toSet,edges.toSet)
  }


}
