package org.openmole.spatialdata.utils.osm

import java.io.Serializable

import org.openmole.spatialdata.utils.osm.OSMObject._
import org.openmole.spatialdata.utils.osm.OSMRoot.Enumerator

import scala.collection.mutable



class OSMRoot extends Serializable {
  private var nodes:mutable.Map[Long, Node] = new mutable.HashMap[Long, Node]()
  private var ways:mutable.Map[Long, Way] = new mutable.HashMap[Long, Way]()
  private var relations:mutable.Map[Long, Relation] = new mutable.HashMap[Long, Relation]()

  def removeNode(identity: Long): Node = {
    val node = getNode(identity)
    remove(node)
    node
  }

  def removeWay(identity: Long):Way = {
    val way = getWay(identity)
    remove(way)
    way
  }

  def removeRelation(identity: Long): Relation = {
    val relation = getRelation(identity)
    remove(relation)
    relation
  }

  def enumerateNodes: OSMRoot.Enumerator[Node] = new OSMRoot.Enumerator[Node]() {
    val iterator: Iterator[(Long,Node)] = getNodes.iterator
    override def next: Node = if (iterator.hasNext) iterator.next._2 else null
  }

  def enumerateWays: Enumerator[Way] =
    new OSMRoot.Enumerator[Way]() {
      val iterator: Iterator[(Long,Way)] = getWays.iterator
      override def next: Way = if (iterator.hasNext) iterator.next._2 else null
  }

  def enumerateRelations: Enumerator[Relation] = new OSMRoot.Enumerator[Relation]() {
    val iterator: Iterator[(Long,Relation)] = getRelations.iterator
    override def next: Relation = if (iterator.hasNext) iterator.next._2 else null
  }

  def getNode(identity: Long): Node = getNodes.getOrElse(identity,null)

  def getWay(identity: Long): Way = getWays.getOrElse(identity,null)

  def getRelation(identity: Long): Relation = getRelations.getOrElse(identity,null)

  def remove(`object`: OSMObject): java.util.Set[OSMObject] = {
    val affectedRelations = `object`.accept(removeVisitor)
    affectedRelations
  }

  private val removeVisitor = new RemoveVisitor

  class RemoveVisitor extends OSMObjectVisitor[java.util.Set[OSMObject]] with Serializable {
    override def visit(node: Node): java.util.HashSet[OSMObject] = {
      val affectedRelations = new java.util.HashSet[OSMObject](1024)
      if (node.getWaysMemberships != null) {
        for (way <- node.getWaysMemberships) { // need to loop in case we visit this node multiple times, eg a polygon where this is start and stop
          while ( {
            way.getNodes.contains(node)
          }) way.getNodes.remove(way.getNodes.indexOf(node))
          affectedRelations.add(way)
        }
      }
      node.setWaysMemberships(null)
      if (node.getRelationMemberships != null) {
        for (member <- node.getRelationMemberships) {
          member.getRelation.getMembers.remove(member.getRelation.getMembers.indexOf(member))
          affectedRelations.add(member.getRelation)
        }
      }
      node.setRelationMemberships(null)
      OSMRoot.this.getNodes.remove(node.getId)
      affectedRelations
    }

    override def visit(way: Way): java.util.Set[OSMObject] = {
      val affectedRelations = new java.util.HashSet[OSMObject](1024)
      if (way.getNodes != null) {
        for (node <- way.getNodes) {
          node.getWaysMemberships.remove(node.getWaysMemberships.indexOf(way))
          affectedRelations.add(node)
        }
        way.setNodes(null)
      }
      if (way.getRelationMemberships != null) {
        for (member <- way.getRelationMemberships) {
          member.getRelation.getMembers.remove(member.getRelation.getMembers.indexOf(member))
          affectedRelations.add(member.getRelation)
        }
        way.setRelationMemberships(null)
      }
      affectedRelations
    }

    override def visit(relation: Relation): java.util.Set[OSMObject] = {
      val affectedRelations = new java.util.HashSet[OSMObject](1024)
      if (relation.getMembers != null) {
        for (member <- relation.getMembers) {
          member.getObject.getRelationMemberships.remove(member.getObject.getRelationMemberships.indexOf(member))
          if (member.getObject.getRelationMemberships.isEmpty) {
            member.getObject.setRelationMemberships(null)
            affectedRelations.add(member.getObject)
          }
        }
        relation.setMembers(null)
      }
      affectedRelations
    }
  }

  @SerialVersionUID(1L)
  class AddVisitor extends OSMObjectVisitor[Void] with Serializable {
    override def visit(node: Node): Null = {
      getNodes.put(node.getId, node)
      null
    }

    override def visit(way: Way): Null = {
      getWays.put(way.getId, way)
      null
    }

    override def visit(relation: Relation): Null = {
      getRelations.put(relation.getId, relation)
      null
    }
  }

  private val addVisitor = new AddVisitor

  def add(osmObject: OSMObject): Unit = {
    osmObject.accept(addVisitor)
  }

  def getNodes: mutable.Map[Long, Node] = nodes

  def setNodes(nodes: mutable.Map[Long, Node]): Unit = {
    this.nodes = nodes
  }

  def getWays: mutable.Map[Long, Way] = ways

  def setWays(ways: mutable.Map[Long, Way]): Unit = {
    this.ways = ways
  }

  def getRelations: mutable.Map[Long, Relation] = relations

  def setRelations(relations: mutable.Map[Long, Relation]): Unit = {
    this.relations = relations
  }

  /**
    * @param f returns true if instance is to be removed from results
    * @return
    */
  def filter(f: OSMObjectVisitor[Boolean]): Iterable[OSMObject] = filter(gatherAllOsmObjects, f)

  def filter(input: Iterable[OSMObject], filter: OSMObjectVisitor[Boolean]): Iterable[OSMObject] =
    input.filter(!_.accept(filter))

  def gatherAllOsmObjects: mutable.HashSet[OSMObject] = {
    val objects = new mutable.HashSet[OSMObject](getWays.size + getRelations.size + getNodes.size,1.0)
    objects.addAll(getWays.values)
    objects.addAll(getRelations.values)
    objects.addAll(getNodes.values)
    objects
  }

  def findNodeByLatitudeAndLongitude(latitude: Double, longitude: Double): mutable.ArrayBuffer[Node] = {
    val nodes = new mutable.ArrayBuffer[Node](100)
    for (node <- getNodes.values) {
      if (node.getLatitude == latitude && node.getLongitude == longitude) nodes.append(node)
    }
    nodes
  }

}

object OSMRoot {
  abstract class Enumerator[T] {
    def next: T
  }
}
