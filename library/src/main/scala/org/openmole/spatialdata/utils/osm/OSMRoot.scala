package org.openmole.spatialdata.utils.osm

import java.io.Serializable

import org.openmole.spatialdata.utils.osm.OSMObject._

import scala.collection.mutable



class OSMRoot extends Serializable {
  val nodes:mutable.Map[Long, Node] = new mutable.HashMap[Long, Node]()
  val ways: mutable.Map[Long, Way] = new mutable.HashMap[Long, Way]()
  val relations:mutable.Map[Long, Relation] = new mutable.HashMap[Long, Relation]()

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

  def getNodes: Seq[Node] = nodes.values.toSeq

  def getWays: Seq[Way] = ways.values.toSeq

  def getRelations: Seq[Relation] = relations.values.toSeq

  def getNode(identity: Long): Node = nodes.getOrElse(identity,null)

  def getWay(identity: Long): Way = ways.getOrElse(identity,null)

  def getRelation(identity: Long): Relation = relations.getOrElse(identity,null)

  def remove(o: OSMObject): java.util.Set[OSMObject] = {
    val affectedRelations = o.accept(removeVisitor)
    affectedRelations
  }

  private val removeVisitor = new RemoveVisitor

  class RemoveVisitor extends OSMObjectVisitor[java.util.Set[OSMObject]] with Serializable {
    override def visit(node: Node): java.util.HashSet[OSMObject] = {
      val affectedRelations = new java.util.HashSet[OSMObject](1024)
      if (node.getWaysMemberships != null) {
        for (way <- node.getWaysMemberships) { // need to loop in case we visit this node multiple times, eg a polygon where this is start and stop
          while ( {
            way.nodes.contains(node)
          }) way.nodes.remove(way.nodes.indexOf(node))
          affectedRelations.add(way)
        }
      }
      node.setWaysMemberships(null)
      if (node.getRelationMemberships != null) {
        for (member <- node.getRelationMemberships) {
          member.getRelation.members.remove(member.getRelation.members.indexOf(member))
          affectedRelations.add(member.getRelation)
        }
      }
      node.setRelationMemberships(null)
      OSMRoot.this.nodes.remove(node.id)
      affectedRelations
    }

    override def visit(way: Way): java.util.Set[OSMObject] = {
      val affectedRelations = new java.util.HashSet[OSMObject](1024)
      if (way.nodes != null) {
        for (node <- way.nodes) {
          node.getWaysMemberships.remove(node.getWaysMemberships.indexOf(way))
          affectedRelations.add(node)
        }
        way.nodes = null
      }
      if (way.getRelationMemberships != null) {
        for (member <- way.getRelationMemberships) {
          member.getRelation.members.remove(member.getRelation.members.indexOf(member))
          affectedRelations.add(member.getRelation)
        }
        way.setRelationMemberships(null)
      }
      affectedRelations
    }

    override def visit(relation: Relation): java.util.Set[OSMObject] = {
      val affectedRelations = new java.util.HashSet[OSMObject](1024)
      if (relation.members != null) {
        for (member <- relation.members) {
          member.getOsmObject.getRelationMemberships.remove(member.getOsmObject.getRelationMemberships.indexOf(member))
          if (member.getOsmObject.getRelationMemberships.isEmpty) {
            member.getOsmObject.setRelationMemberships(null)
            affectedRelations.add(member.getOsmObject)
          }
        }
        relation.members = null
      }
      affectedRelations
    }
  }

  class AddVisitor extends OSMObjectVisitor[Void] with Serializable {
    override def visit(node: Node): Null = {
      nodes.put(node.id, node)
      null
    }

    override def visit(way: Way): Null = {
      ways.put(way.id, way)
      null
    }

    override def visit(relation: Relation): Null = {
      relations.put(relation.id, relation)
      null
    }
  }

  private val addVisitor = new AddVisitor

  def add(osmObject: OSMObject): Unit = {
    osmObject.accept(addVisitor)
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
    objects.addAll(ways.values)
    objects.addAll(relations.values)
    objects.addAll(nodes.values)
    objects
  }

  def findNodeByLatitudeAndLongitude(latitude: Double, longitude: Double): mutable.ArrayBuffer[Node] = {
    val res = new mutable.ArrayBuffer[Node](100)
    for (node <- nodes.values) {
      if (node.getLatitude == latitude && node.getLongitude == longitude) res.append(node)
    }
    res
  }

}


