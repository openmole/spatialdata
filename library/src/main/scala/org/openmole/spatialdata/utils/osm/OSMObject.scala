package org.openmole.spatialdata.utils.osm

import java.io.Serializable

import scala.beans.BeanProperty
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer




abstract class OSMObject extends Serializable {

  def accept[R](visitor: OSMObject.OSMObjectVisitor[R]): R

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
  private var relationMemberships: mutable.ArrayBuffer[OSMObject.RelationMembership] = _

  def addRelationMembership(member: OSMObject.RelationMembership): Unit = {
    if (relationMemberships == null) relationMemberships = new ArrayBuffer[OSMObject.RelationMembership](5)
    else { // don't add membership to the same object twice
      for (relationMembership <- relationMemberships) {
        if (relationMembership.getRelation == member.getRelation) return
      }
    }
    relationMemberships.addOne(member)
  }

  def getAttributes: mutable.Map[String, String] = attributes

  def setAttributes(attributes: mutable.Map[String, String]): Unit = {
    this.attributes = attributes
  }

  def getRelationMemberships: mutable.ArrayBuffer[OSMObject.RelationMembership] = relationMemberships

  def setRelationMemberships(relationMemberships: mutable.ArrayBuffer[OSMObject.RelationMembership]): Unit = {
    this.relationMemberships = relationMemberships
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

  def getVersion: Integer = version

  def setVersion(version: Integer): Unit = {
    this.version = version
  }

  def getChangeset: Long = changeset

  def setChangeset(changeset: Long): Unit = {
    this.changeset = changeset
  }

  def getUser: String = user

  def setUser(user: String): Unit = {
    this.user = user
  }

  def isVisible: Boolean = visible

  def setVisible(visible: Boolean): Unit = {
    this.visible = visible
  }

  def getId: Long = id

  def setId(id: Long): Unit = {
    this.id = id
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

  class Node() extends OSMObject with Serializable {
    def accept[R](visitor: OSMObjectVisitor[R]): R = visitor.visit(this)

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


  class Way extends OSMObject with Serializable {

    override def accept[R](visitor: OSMObjectVisitor[R]): R = visitor.visit(this)

    def Way(id: Long): Unit = {
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

    override def toString: String = "Way{" + super.toString + "nodes.size=" + (if (nodes == null) "null" else nodes.size) + '}'
  }


  class Relation extends OSMObject with Serializable {
    override def accept[R](visitor: OSMObjectVisitor[R]): R = visitor.visit(this)

    private var members: mutable.ArrayBuffer[RelationMembership] = _

    def addMember(member: RelationMembership): mutable.ArrayBuffer[RelationMembership] = {
      if (members == null) members = new mutable.ArrayBuffer[RelationMembership](50)
      members.append(member)
    }

    def getMembers: mutable.ArrayBuffer[RelationMembership] = members

    def setMembers(members: mutable.ArrayBuffer[RelationMembership]): Unit = {
      this.members = members
    }

    override def toString: String = "Relation{" + super.toString + "members=" + members + '}'
  }


  class RelationMembership extends Serializable {
    private var relation: Relation = _
    private var `object`: OSMObject = _
    /**
      * todo intern in domain!
      */
    private var role: String = _

    def getRelation: Relation = relation

    def setRelation(relation: Relation): Unit = {
      this.relation = relation
    }

    def getObject: OSMObject = `object`

    def setObject(`object`: OSMObject): Unit = {
      this.`object` = `object`
    }

    def getRole: String = role

    def setRole(role: String): Unit = {
      this.role = role
    }

    override def toString: String = "RelationMembership{" + "role='" + role + '\'' + ", relation.id=" + (if (relation != null) relation.getId
    else "null") + ", object.id=" + (if (`object` != null) `object`.getId
    else "null") + '}'
  }

  class NotLoadedException(detailMessage: String, throwable: Throwable) extends java.lang.RuntimeException(detailMessage, throwable) {
    def this(`object`: OSMObject) {
      this(`object`.getClass.getSimpleName + "#id " + `object`.getId + " is not loaded!", null)
    }
    def this(detailMessage: String) {
      this(detailMessage, null)
    }
  }


}
