package org.openmole.spatialdata.utils.osm


object Root {
  abstract class Enumerator[T] {
    def next: T
  }
}


trait Root {
  def add(osmObject: OsmObject): Unit

  def getNode(identity: Long): Node

  def getWay(identity: Long): Way

  def getRelation(identity: Long): Relation

  def enumerateNodes: Root.Enumerator[Node]

  def enumerateWays: Root.Enumerator[Way]

  def enumerateRelations: Root.Enumerator[Relation]

  def remove(osmObject: OsmObject): java.util.Set[OsmObject]

  def removeNode(identity: Long): Node

  def removeWay(identity: Long): Way

  def removeRelation(identity: Long):Relation
}
