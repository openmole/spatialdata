package org.openmole.spatialdata.utils.osm

import java.io.Serializable
import java.util.{ArrayList, List}

import scala.beans.BeanProperty



trait OsmObjectVisitor[R] {
  def visit(node: Node): R

  def visit(way: Way): R

  def visit(relation: Relation): R
}


abstract class OsmObject extends Serializable {
  def accept[R](visitor: OsmObjectVisitor[R]): R

  /**
    * if true, then this object has not been loaded, it's just a referenced object
    */
  private var loaded = false
  protected var id = 0L
  private var attributes:java.util.Map[String, String] = _
  private var version:Integer = _
  private var changeset = 0L
  private var uid = 0L
  private var user:String = _
  private var visible = false
  private var timestamp = 0L
  private var tags:java.util.Map[String, String] = _
  private var relationMemberships:java.util.List[RelationMembership] = _

  def addRelationMembership(member: RelationMembership): Unit = {
    if (relationMemberships == null) relationMemberships = new java.util.ArrayList[RelationMembership](5)
    else { // don't add membership to the same object twice
      import scala.collection.JavaConversions._
      for (relationMembership <- relationMemberships) {
        if (relationMembership.getRelation == member.getRelation) return
      }
    }
    relationMemberships.add(member)
  }

  def getAttributes = attributes

  def setAttributes(attributes: java.util.Map[String, String]) = {
    this.attributes = attributes
  }

  def getRelationMemberships = relationMemberships

  def setRelationMemberships(relationMemberships: java.util.List[RelationMembership]) = {
    this.relationMemberships = relationMemberships
  }

  def getAttribute(key: String): String = {
    if (attributes == null) return null
    attributes.get(key)
  }

  def setAttribute(key: String, value: String) = {
    if (attributes == null) attributes = new java.util.HashMap[String, String]()
    attributes.put(key, value)
  }

  def getTag(key: String): String = {
    if (tags == null) return null
    tags.get(key)
  }

  def setTag(key: String, value: String) = {
    if (tags == null) {
      tags = new java.util.HashMap[String, String]()
    }
    tags.put(key, value)
  }

  def getVersion = version

  def setVersion(version: Integer) = {
    this.version = version
  }

  def getChangeset = changeset

  def setChangeset(changeset: Long) = {
    this.changeset = changeset
  }

  def getUser = user

  def setUser(user: String) = {
    this.user = user
  }

  def isVisible = visible

  def setVisible(visible: Boolean) = {
    this.visible = visible
  }

  def getId = id

  def setId(id: Long) = {
    this.id = id
  }

  def getTags = tags

  def setTags(tags: java.util.Map[String, String]) = {
    this.tags = tags
  }

  def getTimestamp = timestamp

  def setTimestamp(timestamp: Long) = {
    this.timestamp = timestamp
  }

  def getUid = uid

  def setUid(uid: Long) = {
    this.uid = uid
  }

  def isLoaded = loaded

  def setLoaded(loaded: Boolean) = {
    this.loaded = loaded
  }

  override def toString = "OsmObject{" + "loaded=" + loaded + ", id=" + id + ", attributes=" + attributes + ", version=" + version + ", changeset=" + changeset + ", uid=" + uid + ", user='" + user + '\'' + ", visible=" + visible + ", timestamp=" + timestamp + ", tags=" + tags + ", relationMemberships=" + relationMemberships + '}'
}



class Node() extends OsmObject with Serializable {
  def accept[R](visitor: OsmObjectVisitor[R]) = visitor.visit(this)

  def this(id: Long, latitude: Double, longitude: Double, tags: String*) {
    this()
    this.latitude = latitude
    this.longitude = longitude
    setId(id)
    var i = 0
    while ( {
      i < tags.length
    }) {
      setTag(tags(i), tags(i + 1))

      i += 2
    }
  }

  private var latitude = .0
  private var longitude = .0
  private var waysMemberships: java.util.List[Way] = _

  def addWayMembership(way: Way): Unit = {
    if (waysMemberships == null) waysMemberships = new java.util.ArrayList[Way](5)
    else { // don't add membership to the same way twice
      // this happens for instance when this is the start and stop in a polygon.
      import scala.collection.JavaConversions._
      for (wayMembership <- waysMemberships) {
        if (way == wayMembership) return
      }
    }
    waysMemberships.add(way)
  }

  def getWaysMemberships = waysMemberships

  def setWaysMemberships(waysMemberships: java.util.List[Way]) = {
    this.waysMemberships = waysMemberships
  }

  def getLatitude = latitude

  def setLatitude(latitude: Double) = {
    this.latitude = latitude
  }

  def getLongitude = longitude

  def setLongitude(longitude: Double) = {
    this.longitude = longitude
  }

  def getY = getLatitude

  def setY(latitude: Double) = {
    setLatitude(latitude)
  }

  def getX = getLongitude

  def setX(longitude: Double) = {
    setLongitude(longitude)
  }

  override def toString = "Node{" + super.toString + "latitude=" + latitude + ", longitude=" + longitude + ", waysMemberships.size=" + (if (waysMemberships == null) "null"
  else waysMemberships.size) + '}'
}




class Way extends OsmObject with Serializable {

  override def accept[R](visitor: OsmObjectVisitor[R]): R = visitor.visit(this)

  def Way(id: Long) = {
    this.id = id
  }

  @BeanProperty
  var nodes: List[Node] = _

  /**
    * @return true if an enclosed polygon
    */
  def isPolygon: Boolean = {
    if (!isLoaded) {
      throw new NotLoadedException(this)
    }
    getNodes.size > 2 && getNodes.get(0) == getNodes.get(getNodes.size - 1)
  }

  def addNode(node: Node): Way = {
    if (nodes == null) {
      nodes = new ArrayList[Node](50)
    }
    nodes.add(node)
    this
  }

  override def toString(): String = "Way{" + super.toString + "nodes.size=" + (if (nodes == null) "null" else nodes.size) + '}'
}




class Relation extends OsmObject with Serializable {
  override def accept[R](visitor: OsmObjectVisitor[R]) = visitor.visit(this)

  private var members:java.util.List[RelationMembership] = null

  def addMember(member: RelationMembership) = {
    if (members == null) members = new java.util.ArrayList[RelationMembership](50)
    members.add(member)
  }

  def getMembers = members

  def setMembers(members: java.util.List[RelationMembership]) = {
    this.members = members
  }

  override def toString = "Relation{" + super.toString + "members=" + members + '}'
}


