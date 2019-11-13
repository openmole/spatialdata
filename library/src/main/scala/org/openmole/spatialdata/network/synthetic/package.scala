package org.openmole.spatialdata.network

package object synthetic {


  /**
    * very simple network for algo testing
    * @return
    */
  def crossNetwork: Network = {
    val (n0,n1,n2,n3,n4) = (
      Node(0,0.0,0.0),
      Node(1,0.0,1.0),
      Node(2,1.0,0.0),
      Node(3,0.0,-1.0),
      Node(4,-1.0,0.0)
    )
    val nodes = Set(n0,n1,n2,n3,n4)
    val links = Set(Link(n0,n1),Link(n0,n2),Link(n0,n3),Link(n0,n4))
    Network(nodes,links)
  }



}
