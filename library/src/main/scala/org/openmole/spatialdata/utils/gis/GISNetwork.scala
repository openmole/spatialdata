package org.openmole.spatialdata.utils.gis

import org.openmole.spatialdata.network.{Link, Network, Node}
import org.openmole.spatialdata.utils.math.Implicits._
import org.openmole.spatialdata.vector.{Lines, Points}

object GISNetwork {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  /**
    * Import a network from gis linestrings
    *   - brutal algorithm with aggregation of vertices on an underlying grid
    *   - network is undirected
    *
    *   enh:
    *   - LineStrings should have some attributes: weight ok taken into account; other attributes: need to extend Network
    *   - should similar lines be duplicated ? for perf better to have a Set
    *
    * @param lines Seq of linestrings to be converted to a network
    * @param snap spatial tolerance for node aggregation
    * @return
    */
  def networkFromGISLinesGridSnapping(lines: Lines, snap: Double, weightAttribute: String = ""): Network = {
    val envelopes = lines.lines.map{l => val env =l.getEnvelopeInternal; (env.getMinX,env.getMaxX,env.getMinY,env.getMaxY)}
    val (xmin,xmax,ymin,ymax) = (envelopes.map{_._1}.min,envelopes.map{_._2}.max,envelopes.map{_._3}.min,envelopes.map{_._4}.max)
    def icoord(x: Double): Int = math.floor((x - xmin)/(snap*(xmax-xmin))).toInt
    def jcoord(y: Double): Int = math.floor((y - ymin)/(snap*(ymax-ymin))).toInt
    // implement in a functional way - different performance than with a mutable HashMap?
    def addLine(state: (Lines,Set[(Int,Int)],Set[(Int,Int,Int,Int)],Map[(Int,Int,Int,Int),Double])): (Lines,Set[(Int,Int)],Set[(Int,Int,Int,Int)],Map[(Int,Int,Int,Int),Double]) = {
      if (state._1.lines.isEmpty) return state // nothing to add
      val coords = state._1.lines.head.getCoordinates
      val links: Set[(Int,Int,Int,Int)] = coords.dropRight(1).zip(coords.tail).map{
        case (p1,p2) =>
          val (i1,j1,i2,j2) = (icoord(p1.x),jcoord(p1.y),icoord(p2.x),jcoord(p2.y))
          if ((i1,j1)==(i2,j2)) None else {
            if ((i1, j1) <= (i2, j2)) Some((i1, j1, i2, j2)) else Some((i2, j2,i1, j1)) // sorting allows to not duplicate links in inverse direction
          }
      }.flatten.toSet
      val nodes = links.flatMap{l=>Set((l._1,l._2),(l._3,l._4))}
      (state._1.tail,state._2++nodes,state._3++links,state._4++links.map(_ -> (if (weightAttribute.length>0) state._1.attributes.head.getOrElse(weightAttribute,1.0).asInstanceOf[Double] else 1.0)))
    }
    // all nodes are in the links by construction
    val (_,_,alllinks,weights) = Iterator.iterate((lines,Set.empty[(Int,Int)],Set.empty[(Int,Int,Int,Int)],Map.empty[(Int,Int,Int,Int),Double]))(addLine).takeWhile(_._1.lines.nonEmpty).toSeq.last
    // correct indexation of node done in the constructor
    Network(alllinks.map{l => Link(Node(0,xmin+l._1*snap,ymin+l._2*snap),Node(0,xmin+l._3*snap,ymin+l._4*snap),weight = weights((l._1,l._2,l._3,l._4)))})
  }


  /**
    * Given a network from GIS lines, adds "station" nodes attributes
    * (rq: the function is generic to any attribute and any additional node layer)
    * @param network network
    * @return
    */
  def addStationNodes(network: Network, attributeNodes: Points): Network = {
    Network.empty
  }



}
