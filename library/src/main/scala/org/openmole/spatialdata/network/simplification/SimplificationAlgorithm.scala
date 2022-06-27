package org.openmole.spatialdata.network.simplification

import org.openmole.spatialdata.network.{Link, Network, Node}
import org.openmole.spatialdata.network.measures.NetworkMeasures
import org.openmole.spatialdata.utils

import scala.collection.mutable


object SimplificationAlgorithm {


  /**
   * Remove nodes with degree equal to two
   * ! fails if network has self-loops -> remove these first
   *
   * implemented iteratively with mutable Sets for performance
   *
   * @param network network
   * @param combineLength function to combine lengths
   * @return
   */
  def simplifyNetwork(network: Network,
                      combineLength: (Link,Link)=>Double = {case (l1,l2)=>l1.length+l2.length},
                      combineWeights: (Link,Link)=>Double = {case (l1,l2)=>(l1.weight*l1.length + l2.weight*l2.length)/(l1.length+l2.length)}
                     ): Network = {
    // remove self loops first
    val selfloops = network.links.filter(_.isSelfLoop)
    utils.log(s"Network simplification: removing ${selfloops.size} self-loops")
    val nw = network.removeLinks(selfloops, keepNodes = true)
    val nodes = mutable.HashSet.from(nw.nodes)
    val links = mutable.HashSet.from(nw.links)
    val nodeLinkMap = new mutable.HashMap[Node,Set[Link]]
    links.foreach{l => nodeLinkMap.put(l.e1,nodeLinkMap.getOrElse(l.e1,Set.empty[Link])++Set(l)); nodeLinkMap.put(l.e2,nodeLinkMap.getOrElse(l.e2,Set.empty[Link])++Set(l))}
    val degrees = mutable.HashMap.from(NetworkMeasures.degreeDistribution(nw)) // note: could not recompute degree, ~ same
    utils.log(s"Network simplification: ${degrees.values.count(_==2)} nodes to remove")
    while(degrees.values.count(_==2)>0) {
      if (degrees.size % 1000 == 0) utils.log(s"${degrees.size}")
      // not performant to do a toSeq?
      val toremove = degrees.toSeq.filter(_._2 == 2).head._1
      nodes.remove(toremove)
      //println("degree="+degrees(toremove))
      degrees.remove(toremove)
      val replacedlinks = nodeLinkMap(toremove).toSeq
      //println("toremove : "+toremove)
      //println("replaced links : "+replacedlinks)
      nodeLinkMap.remove(toremove)
      replacedlinks.foreach { l =>
        links.remove(l)
        nodeLinkMap.put(l.e1, nodeLinkMap.getOrElse(l.e1, Set(l)) -- Set(l))
        nodeLinkMap.put(l.e2, nodeLinkMap.getOrElse(l.e2, Set(l)) -- Set(l))
      }

      val othernodes = replacedlinks.flatMap(l => Set(l.e1, l.e2)).filter(_ != toremove)
      if (othernodes.size >= 2) { // why empty in some cases? -> node alone?
        //println("othernodes : "+othernodes)
        // this will fail if there are self loops
        assert(othernodes.size == 2, s"In network simplification: removed vertex had not two neighbors: $othernodes, replaced links: $replacedlinks, self-loops: ${replacedlinks.map(_.isSelfLoop)}")
        val newlink = Link(othernodes.head, othernodes(1), combineWeights(replacedlinks.head, replacedlinks(1)), combineLength(replacedlinks.head, replacedlinks(1)), directed = false)
        // add newlink if not self-loop
        if (!newlink.isSelfLoop) {
          links.add(newlink)
          //println("new link : "+newlink+"\n")
          //assert(othernodes.size==2,"In network simplification: removed vertice had not two neighbors")
          nodeLinkMap.put(othernodes.head, nodeLinkMap.getOrElse(othernodes.head, Set.empty[Link]) ++ Set(newlink))
          nodeLinkMap.put(othernodes(1), nodeLinkMap.getOrElse(othernodes(1), Set.empty[Link]) ++ Set(newlink))
        }
        // no need to update the degree
      }
    }
    Network(nodes.toSet,links.toSet)
  }


}
