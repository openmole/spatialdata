package org.openmole.spatialdata.utils.osm

import java.io.Serializable

import scala.beans.BeanProperty
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer




abstract case class OSMObject(
                             id: Long
                             ) extends Serializable {

  def accept[R](visitor: OSMObject.OSMObjectVisitor[R]): R

  /**
    * if true, then this object has not been loaded, it's just a referenced object
    */
  private var loaded = false


  var attributes: mutable.Map[String, String] = _
  def setAttributes(map: mutable.Map[String, String]): Unit = {this.attributes = map}
  def getAttributes: mutable.Map[String, String] = attributes

  var version: Integer = _
  def setVersion(v: Integer): Unit = {this.version = v}
  def getVersion: Integer = version

  var changeset = 0L
  def setChangeset(l: Long): Unit = {this.changeset = l}
  def getChangeset: Long = changeset

  private var uid = 0L

  var user: String = _
  def setUser(u: String): Unit = {this.user = u}
  def getUser: String = user

  var visible = false
  def setVisible(b: Boolean): Unit = {this.visible = b}
  def getVisible: Boolean = visible

  private var timestamp = 0L
  private var tags: mutable.Map[String, String] = _

  var relationMemberships: mutable.ArrayBuffer[OSMObject.RelationMembership] = _
  def setRelationMemberships(m: mutable.ArrayBuffer[OSMObject.RelationMembership]): Unit = {this.relationMemberships = m}
  def getRelationMemberships: mutable.ArrayBuffer[OSMObject.RelationMembership] = relationMemberships

  def addRelationMembership(member: OSMObject.RelationMembership): Unit = {
    if (relationMemberships == null) relationMemberships = new ArrayBuffer[OSMObject.RelationMembership](5)
    else { // don't add membership to the same object twice
      for (relationMembership <- relationMemberships) {
        if (relationMembership.getRelation == member.getRelation) return
      }
    }
    relationMemberships.addOne(member)
  }


  def getAttribute(key: String): String = {
    if (attributes == null) return null
    attributes.getOrElse(key,null)
  }

  def setAttribute(key: String, value: String): Option[String] = {
    if (attributes == null) attributes = new mutable.HashMap[String, String]
    attributes.put(key, value)
  }

  def getTag(key: String): String = {
    if (tags == null) return null
    tags.getOrElse(key, null)
  }

  def setTag(key: String, value: String): Option[String] = {
    if (tags == null) {
      tags = new mutable.HashMap[String, String]
    }
    tags.put(key, value)
  }

  def getTags: mutable.Map[String, String] = tags

  def setTags(tags: mutable.Map[String, String]): Unit = {
    this.tags = tags
  }

  def getTimestamp: Long = timestamp

  def setTimestamp(timestamp: Long): Unit = {
    this.timestamp = timestamp
  }

  def getUid: Long = uid

  def setUid(uid: Long): Unit = {
    this.uid = uid
  }

  def isLoaded: Boolean = loaded

  def setLoaded(loaded: Boolean): Unit = {
    this.loaded = loaded
  }

  override def toString: String = "OsmObject{" + "loaded=" + loaded + ", id=" + id + ", attributes=" + attributes + ", version=" + version + ", changeset=" + changeset + ", uid=" + uid + ", user='" + user + '\'' + ", visible=" + visible + ", timestamp=" + timestamp + ", tags=" + tags + ", relationMemberships=" + relationMemberships + '}'
}


object OSMObject {

  trait OSMObjectVisitor[R] {
    def visit(node: Node): R

    def visit(way: Way): R

    def visit(relation: Relation): R
  }

  class Node(id: Long) extends OSMObject(id) with Serializable {
    def accept[R](visitor: OSMObjectVisitor[R]): R = visitor.visit(this)

    def this(id: Long, latitude: Double, longitude: Double, tags: String*) = {
      this(id)
      this.latitude = latitude
      this.longitude = longitude
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

    def getWaysMemberships: mutable.ArrayBuffer[Way] = waysMemberships

    def setWaysMemberships(waysMemberships: mutable.ArrayBuffer[Way]): Unit = {
      this.waysMemberships = waysMemberships
    }

    def getLatitude: Double = latitude

    def setLatitude(latitude: Double): Unit = {
      this.latitude = latitude
    }

    def getLongitude: Double = longitude

    def setLongitude(longitude: Double): Unit = {
      this.longitude = longitude
    }

    def getY: Double = getLatitude

    def setY(latitude: Double): Unit = {
      setLatitude(latitude)
    }

    def getX: Double = getLongitude

    def setX(longitude: Double): Unit = {
      setLongitude(longitude)
    }

    override def toString: String = "Node{" + super.toString + "latitude=" + latitude + ", longitude=" + longitude + ", waysMemberships.size=" + (if (waysMemberships == null) "null"
    else waysMemberships.size) + '}'
  }


  class Way(id: Long) extends OSMObject(id) with Serializable {

    override def accept[R](visitor: OSMObjectVisitor[R]): R = visitor.visit(this)

    var nodes: mutable.ArrayBuffer[Node] = new mutable.ArrayBuffer[Node]

    /**
      * @return true if an enclosed polygon
      */
    def isPolygon: Boolean = {
      if (!isLoaded) {
        throw new NotLoadedException(this)
      }
      nodes.size > 2 && nodes(0) == nodes(nodes.size - 1)
    }

    def addNode(node: Node): Way = {
      nodes.append(node)
      this
    }

    override def toString: String = "Way{" + super.toString + "nodes.size=" + (if (nodes == null) "null" else nodes.size) + '}'
  }


  class Relation(id: Long) extends OSMObject(id) with Serializable {
    override def accept[R](visitor: OSMObjectVisitor[R]): R = visitor.visit(this)

    var members: mutable.ArrayBuffer[RelationMembership] = new mutable.ArrayBuffer[RelationMembership]

    def addMember(member: RelationMembership): mutable.ArrayBuffer[RelationMembership] =
      members.append(member)

    override def toString: String = "Relation{" + super.toString + "members=" + members + '}'
  }


  class RelationMembership extends Serializable {
    private var relation: Relation = _

    var osmObject: OSMObject = _
    def setOsmObject(o: OSMObject): Unit = {this.osmObject = o}
    def getOsmObject: OSMObject = this.osmObject
    /**
      * todo intern in domain!
      */
    private var role: String = _

    def getRelation: Relation = relation
    def setRelation(relation: Relation): Unit = {this.relation = relation}

    def getRole: String = role
    def setRole(role: String): Unit = {this.role = role}

    override def toString: String = "RelationMembership{" + "role='" + role + '\'' + ", relation.id=" + (if (relation != null) relation.id
    else "null") + ", object.id=" + (if (osmObject != null) osmObject.id
    else "null") + '}'
  }

  class NotLoadedException(detailMessage: String, throwable: Throwable) extends java.lang.RuntimeException(detailMessage, throwable) {
    def this(o: OSMObject) = {
      this(o.getClass.getSimpleName + "#id " + o.id + " is not loaded!", null)
    }
    def this(detailMessage: String) =  {
      this(detailMessage, null)
    }
  }


}
