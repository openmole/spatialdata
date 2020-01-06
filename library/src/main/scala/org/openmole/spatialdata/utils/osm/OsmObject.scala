package org.openmole.spatialdata.utils.osm

import java.io.Serializable
import java.util.{ArrayList, List}

import scala.beans.BeanProperty
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



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
  private var attributes: mutable.Map[String, String] = _
  private var version:Integer = _
  private var changeset = 0L
  private var uid = 0L
  private var user:String = _
  private var visible = false
  private var timestamp = 0L
  private var tags: mutable.Map[String, String] = _
  private var relationMemberships:mutable.ArrayBuffer[RelationMembership] = _

  def addRelationMembership(member: RelationMembership): Unit = {
    if (relationMemberships == null) relationMemberships = new ArrayBuffer[RelationMembership](5)
    else { // don't add membership to the same object twice
      for (relationMembership <- relationMemberships) {
        if (relationMembership.getRelation == member.getRelation) return
      }
    }
    relationMemberships.addOne(member)
  }

  def getAttributes = attributes

  def setAttributes(attributes: mutable.Map[String, String]) = {
    this.attributes = attributes
  }

  def getRelationMemberships = relationMemberships

  def setRelationMemberships(relationMemberships: mutable.ArrayBuffer[RelationMembership]) = {
    this.relationMemberships = relationMemberships
  }

  def getAttribute(key: String): String = {
    if (attributes == null) return null
    attributes.getOrElse(key,null)
  }

  def setAttribute(key: String, value: String) = {
    if (attributes == null) attributes = new mutable.HashMap[String, String]
    attributes.put(key, value)
  }

  def getTag(key: String): String = {
    if (tags == null) return null
    tags.getOrElse(key, null)
  }

  def setTag(key: String, value: String) = {
    if (tags == null) {
      tags = new mutable.HashMap[String, String]
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

  def setTags(tags: mutable.Map[String, String]) = {
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
  private var waysMemberships: mutable.ArrayBuffer[Way] = _

  def addWayMembership(way: Way): Unit = {
    if (waysMemberships == null) waysMemberships = new mutable.ArrayBuffer[Way](5)
    else { // don't add membership to the same way twice
      // this happens for instance when this is the start and stop in a polygon.
      for (wayMembership <- waysMemberships) {
        if (way == wayMembership) return
      }
    }
    waysMemberships.append(way)
  }

  def getWaysMemberships = waysMemberships

  def setWaysMemberships(waysMemberships: mutable.ArrayBuffer[Way]) = {
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
  var nodes: mutable.ArrayBuffer[Node] = _

  /**
    * @return true if an enclosed polygon
    */
  def isPolygon: Boolean = {
    if (!isLoaded) {
      throw new NotLoadedException(this)
    }
    getNodes.size > 2 && getNodes()(0) == getNodes()(getNodes.size - 1)
  }

  def addNode(node: Node): Way = {
    if (nodes == null) {
      nodes = new mutable.ArrayBuffer[Node](50)
    }
    nodes.append(node)
    this
  }

  override def toString(): String = "Way{" + super.toString + "nodes.size=" + (if (nodes == null) "null" else nodes.size) + '}'
}




class Relation extends OsmObject with Serializable {
  override def accept[R](visitor: OsmObjectVisitor[R]) = visitor.visit(this)

  private var members: mutable.ArrayBuffer[RelationMembership] = null

  def addMember(member: RelationMembership) = {
    if (members == null) members = new mutable.ArrayBuffer[RelationMembership](50)
    members.append(member)
  }

  def getMembers = members

  def setMembers(members: mutable.ArrayBuffer[RelationMembership]) = {
    this.members = members
  }

  override def toString = "Relation{" + super.toString + "members=" + members + '}'
}


