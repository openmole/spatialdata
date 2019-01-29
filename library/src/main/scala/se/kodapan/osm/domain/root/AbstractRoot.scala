package se.kodapan.osm.domain.root

import se.kodapan.osm.domain.Node
import se.kodapan.osm.domain.Relation
import se.kodapan.osm.domain.Way
import java.io.Serializable


/**
  * @author kalle
  * @since 2013-10-20 04:38
  */
@SerialVersionUID(1l)
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
