
package spatialdata.synthetic.network

import spatialdata.network._

import scala.collection.mutable.ArrayBuffer

import scala.util.Random

case class GridNetworkGenerator(
                               size: Int,
                               xstep: Int = 1,
                               ystep: Int = 1,
                               withDiagLinks: Boolean = false
                               ) extends NetworkGenerator {

  override def generateNetwork(implicit rng: Random): Network = GridNetworkGenerator.gridNetwork(xstep=xstep,ystep=ystep,size=size,diagLinks = withDiagLinks)

}


object GridNetworkGenerator {

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
    //println(nodes.size)
    //println(nodes(0).size)
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
    //println("grid nw links = "+edges.size)
    //println("grid nw unique links = "+edges.map{case e => e.e1.id+"-"+e.e2.id+"-"+e.weight}.size)
    Network(nodes.flatten.toSet,edges.toSet)
  }


}
