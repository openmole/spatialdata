package org.openmole.spatialdata.utils.osm

import java.io.Serializable


class PojoRoot extends AbstractRoot with Serializable {
  private var nodes:java.util.Map[Long, Node] = new java.util.HashMap[Long, Node]()
  private var ways:java.util.Map[Long, Way] = new java.util.HashMap[Long, Way]()
  private var relations:java.util.Map[Long, Relation] = new java.util.HashMap[Long, Relation]()

  override def enumerateNodes = new Root.Enumerator[Node]() {
    val iterator = getNodes.entrySet.iterator
    override

    def next: Node = return if (iterator.hasNext) iterator.next.getValue
    else null
  }

  override def enumerateWays = //getWays.values().iterator
    new Root.Enumerator[Way]() {val iterator = getWays.entrySet.iterator
    override def next: Way =
      return if (iterator.hasNext) iterator.next.getValue else null
  }

  override def enumerateRelations = new Root.Enumerator[Relation]() {
    val iterator = getRelations.entrySet.iterator
    override def next: Relation = return if (iterator.hasNext) iterator.next.getValue
    else null
  }

  override def getNode(identity: Long) = getNodes.get(identity)

  override def getWay(identity: Long) = getWays.get(identity)

  override def getRelation(identity: Long) = getRelations.get(identity)

  override def remove(`object`: OsmObject): java.util.Set[OsmObject] = {
    val affectedRelations = `object`.accept(removeVisitor)
    affectedRelations
  }

  private val removeVisitor = new RemoveVisitor

  class RemoveVisitor extends OsmObjectVisitor[java.util.Set[OsmObject]] with Serializable {
    override def visit(node: Node) = {
      val affectedRelations = new java.util.HashSet[OsmObject](1024)
      if (node.getWaysMemberships != null) {
        import scala.collection.JavaConversions._
        for (way <- node.getWaysMemberships) { // need to loop in case we visit this node multiple times, eg a polygon where this is start and stop
          while ( {
            way.getNodes.contains(node)
          }) way.getNodes.remove(node)
          affectedRelations.add(way)
        }
      }
      node.setWaysMemberships(null)
      if (node.getRelationMemberships != null) {
        import scala.collection.JavaConversions._
        for (member <- node.getRelationMemberships) {
          member.getRelation.getMembers.remove(member)
          affectedRelations.add(member.getRelation)
        }
      }
      node.setRelationMemberships(null)
      PojoRoot.this.getNodes.remove(node.getId)
      affectedRelations
    }

    override def visit(way: Way) = {
      val affectedRelations = new java.util.HashSet[OsmObject](1024)
      if (way.getNodes != null) {
        import scala.collection.JavaConversions._
        for (node <- way.getNodes) {
          node.getWaysMemberships.remove(way)
          affectedRelations.add(node)
        }
        way.setNodes(null)
      }
      if (way.getRelationMemberships != null) {
        import scala.collection.JavaConversions._
        for (member <- way.getRelationMemberships) {
          member.getRelation.getMembers.remove(member)
          affectedRelations.add(member.getRelation)
        }
        way.setRelationMemberships(null)
      }
      affectedRelations
    }

    override def visit(relation: Relation) = {
      val affectedRelations = new java.util.HashSet[OsmObject](1024)
      if (relation.getMembers != null) {
        import scala.collection.JavaConversions._
        for (member <- relation.getMembers) {
          member.getObject.getRelationMemberships.remove(member)
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

  @SerialVersionUID(1l)
  class AddVisitor extends OsmObjectVisitor[Void] with Serializable {
    override def visit(node: Node) = {
      getNodes.put(node.getId, node)
      null
    }

    override def visit(way: Way) = {
      getWays.put(way.getId, way)
      null
    }

    override def visit(relation: Relation) = {
      getRelations.put(relation.getId, relation)
      null
    }
  }

  private val addVisitor = new AddVisitor

  override def add(osmObject: OsmObject) = {
    osmObject.accept(addVisitor)
  }

  def getNodes = nodes

  def setNodes(nodes: java.util.Map[Long, Node]) = {
    this.nodes = nodes
  }

  def getWays = ways

  def setWays(ways: java.util.Map[Long, Way]) = {
    this.ways = ways
  }

  def getRelations = relations

  def setRelations(relations: java.util.Map[Long, Relation]) = {
    this.relations = relations
  }

  /**
    * @param filter returns true if instance is to be removed from results
    * @return
    */
  def filter(f: OsmObjectVisitor[Boolean]): java.util.ArrayList[OsmObject] = filter(gatherAllOsmObjects, f)

  def filter(input: java.util.Collection[OsmObject], filter: OsmObjectVisitor[Boolean]) = {
    val response = new java.util.ArrayList[OsmObject](input)
    val iterator = response.iterator
    while ( {
      iterator.hasNext
    }) {
      val `object` = iterator.next
      if (`object`.accept(filter)) iterator.remove()
    }
    response
  }

  def gatherAllOsmObjects = {
    val objects = new java.util.HashSet[OsmObject](getWays.size + getRelations.size + getNodes.size)
    objects.addAll(getWays.values)
    objects.addAll(getRelations.values)
    objects.addAll(getNodes.values)
    objects
  }

  def findNodeByLatitudeAndLongitude(latitude: Double, longitude: Double) = {
    val nodes = new java.util.ArrayList[Node](100)
    import scala.collection.JavaConversions._
    for (node <- getNodes.values) {
      if (node.getLatitude == latitude && node.getLongitude == longitude) nodes.add(node)
    }
    nodes
  }

  /**
    * todo this loop get really slow where there is hundreds of thousands of coordinates and as many calls to the method... index?
    * todo if an index then it needs to be up to date when node values change. that requires keeping track of root from osmobjects....
    * todo and what if the object is added to multiple roots then?
    * todo so an index is probably not possible.
    */
  def findFirstNodeByLatitudeAndLongitude(latitude: Double, longitude: Double): Node = {
    import scala.collection.JavaConversions._
    for (node <- getNodes.values) {
      if (node.getLatitude == latitude && node.getLongitude == longitude) return node
    }
    null
  }
}
