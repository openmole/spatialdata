package se.kodapan.osm.domain.root

import se.kodapan.osm.domain.Node
import se.kodapan.osm.domain.OsmObject
import se.kodapan.osm.domain.Relation
import se.kodapan.osm.domain.Way
import se.kodapan.osm.domain.root.Root.Enumerator

/**
  * @author kalle
  * @since 2013-10-20 04:35
  */
object Root {

  abstract class Enumerator[T] {
    def next: T
  }

}

trait Root {
  def add(osmObject: OsmObject)

  def getNode(identity: Long): Node

  def getWay(identity: Long): Way

  def getRelation(identity: Long): Relation

  def enumerateNodes: Enumerator[Node]

  def enumerateWays: Enumerator[Way]

  def enumerateRelations: Enumerator[Relation]

  def remove(osmObject: OsmObject): java.util.Set[OsmObject]

  def removeNode(identity: Long): Node

  def removeWay(identity: Long): Way

  def removeRelation(identity: Long):Relation
}
