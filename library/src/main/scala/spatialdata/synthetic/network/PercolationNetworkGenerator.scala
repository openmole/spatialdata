
package spatialdata.synthetic.network

import spatialdata.network._

import scala.util.Random

case class PercolationNetworkGenerator(
                                        worldSize: Int,
                                        percolationProba: Double,
                                        bordPoints: Int,
                                        linkwidth: Double
                                      ) extends NetworkGenerator {
  override def generateNetwork(implicit rng: Random): Network = PercolationNetworkGenerator.bondPercolatedNetwork(worldSize,percolationProba,bordPoints,linkwidth)
}



object PercolationNetworkGenerator {

  /**
    * Basic bond percolation in an overlay network
    * (iterated until having one connected component with a specified number of points on the boundary,
    * keep the largest component at each step)
    * @param worldSize
    * @param percolationProba
    * @return
    */
  def bondPercolatedNetwork(worldSize: Int,percolationProba: Double,bordPoints: Int,linkwidth: Double)(implicit rng: Random): Network = {
    var network = GridNetworkGenerator(worldSize).generateNetwork//.gridNetwork(worldSize/10,worldSize/10,worldSize)
    var bordConnected = 0
    val xmin = network.nodes.map{_.x}.min;val xmax = network.nodes.map{_.x}.max
    val ymin = network.nodes.map{_.y}.min;val ymax = network.nodes.map{_.y}.max
    while(bordConnected<bordPoints){
      network = Network.percolate(network,percolationProba,linkFilter={
        l: Link => l.weight==0.0&&(
          (((l.e1.x!=xmin)&&(l.e2.x!=xmin))||((l.e1.x==xmin)&&(l.e2.x!=xmin))||((l.e2.x==xmin)&&(l.e1.x!=xmin)))&&
            (((l.e1.x!=xmax)&&(l.e2.x!=xmax))||((l.e1.x==xmax)&&(l.e2.x!=xmax))||((l.e2.x==xmax)&&(l.e1.x!=xmax)))&&
            (((l.e1.y!=ymin)&&(l.e2.y!=ymin))||((l.e1.y==ymin)&&(l.e2.y!=ymin))||((l.e2.y==ymin)&&(l.e1.y!=ymin)))&&
            (((l.e1.y!=ymax)&&(l.e2.y!=ymax))||((l.e1.y==ymax)&&(l.e2.y!=ymax))||((l.e2.y==ymax)&&(l.e1.y!=ymax)))
          )
      })
      val giantcomp =  Network.largestConnectedComponent(Network(network.nodes,network.links.filter{_.weight>0}))

      //println("giantcomp size = "+giantcomp.links.size)

      val nodesOnBord = giantcomp.nodes.filter{case n => n.x==xmin||n.x==xmax||n.y==ymin||n.y==ymax}
      bordConnected =nodesOnBord.size

      //println("Percolated links prop : "+(network.links.toSeq.map{_.weight}.sum/network.links.toSeq.size))
      //println("bordConnected = "+bordConnected)
      //println("nodesOnBord="+nodesOnBord)
    }
    network
  }


}

