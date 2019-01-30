package se.kodapan.osm.domain

import java.io.Serializable

/**
  * @author kalle
  * @since 2013-05-01 15:42
  */
@SerialVersionUID(1l)
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
    if (attributes == null) attributes = new java.util.HashMap[String, String](5)
    attributes.put(key, value)
  }

  def getTag(key: String): String = {
    if (tags == null) return null
    tags.get(key)
  }

  def setTag(key: String, value: String) = {
    if (tags == null) tags = new java.util.LinkedHashMap[String, String](5)
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
