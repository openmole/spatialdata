package org.openmole.spatialdata.network


/**
  *
  * Synthetic network generators
  *
  * - Synthetic network generator to implement: https://arxiv.org/abs/2001.08180
  * - Link addition for a bicycle path network: Natera Orozco, L. G., Battiston, F., Iñiguez, G., & Szell, M. (2020). Data-driven strategies for optimal bicycle network growth. Royal Society Open Science, 7(12), 201130.
  * - morphogenesis reaction-diffusion model to implement: Tirico, M., Balev, S., Dutot, A., & Olivier, D. (2018, December). Morphogenesis of complex networks: A reaction diffusion framework for spatial graphs. In International Conference on Complex Networks and their Applications (pp. 769-781). Springer, Cham.
  * - variations in Tirico, M. (2020). Morphogenesis of complex networks.: An application in urban growth (Doctoral dissertation, Normandie).
  * - El Gouj, H., & Lagesse, C. (2021). Road Network Analysis Based on Geo-Historical Data (No. 5453). EasyChair. (extends geometrical model physicists?)
  * - Al-Sayed, K. (2014). Urban morphogenesis; how form-function complexity coupled temporal changes on street configurations in Manhattan and Barcelona over the past centuries (Doctoral dissertation, UCL (University College London)). : model?
  * - Barthelemy, M. (2018). Morphogenesis of spatial networks. Cham, Switzerland: Springer International Publishing
  * - Cats, O., Vermeulen, A., Warnier, M., & van Lint, H. (2020). Modelling growth principles of metropolitan public transport networks. Journal of Transport Geography, 82, 102567..
  * - Synthetic networks? https://decentraland.org/blog/platform/designing-genesis-city-roads-urban-planning/
  */
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

  /**
    *
    * @return
    */
  def spiderNetwork: Network = {
    val (n0,n1,n2,n3,n4) = (Node(0,0.0,0.0), Node(1,0.0,1.0), Node(2,1.0,0.0), Node(3,0.0,-1.0), Node(4,-1.0,0.0))
    val (n5,n6,n7,n8,n9,n10,n11,n12) = (
      Node(5,-1.0,2.0),Node(6,1.0,2.0),Node(7,2.0,1.0),Node(8,2.0,-1.0),Node(9,1.0,-2.0),Node(10,-1.0,-2.0),Node(11,-2.0,-1.0),Node(12,-2.0,1.0)
    )
    val crosslinks = Set(Link(n0,n1),Link(n0,n2),Link(n0,n3),Link(n0,n4))
    val addLinks = Set(Link(n1,n5),Link(n1,n6),Link(n2,n7),Link(n2,n8),Link(n3,n9),Link(n3,n10),Link(n4,n11),Link(n4,n12))
    val nodes = Set(n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12)
    Network(nodes,crosslinks.union(addLinks))
  }



}
