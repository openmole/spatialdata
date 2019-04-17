package org.openmole.spatialdata.utils.osm.domain.root

import java.io.Serializable

import org.openmole.spatialdata.utils.osm.domain._



abstract class AbstractRoot extends Root with Serializable {
  override def removeNode(identity: Long):Node = {
    val node = getNode(identity)
    remove(node)
    node
  }

  override def removeWay(identity: Long):Way = {
    val way = getWay(identity)
    remove(way)
    way
  }

  override def removeRelation(identity: Long): Relation = {
    val relation = getRelation(identity)
    remove(relation)
    relation
  }
}
