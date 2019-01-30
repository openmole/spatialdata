package se.kodapan.osm.domain

import java.io.Serializable


/**
  * @author kalle
  * @since 2013-05-01 21:06
  */
@SerialVersionUID(1l)
class RelationMembership extends Serializable {
  private var relation:Relation = _
  private var `object`:OsmObject = _
  /**
    * todo intern in domain!
    */
  private var role: String = _

  def getRelation = relation

  def setRelation(relation: Relation) = {
    this.relation = relation
  }

  def getObject = `object`

  def setObject(`object`: OsmObject) = {
    this.`object` = `object`
  }

  def getRole = role

  def setRole(role: String) = {
    this.role = role
  }

  override def toString = "RelationMembership{" + "role='" + role + '\'' + ", relation.id=" + (if (relation != null) relation.getId
  else "null") + ", object.id=" + (if (`object` != null) `object`.getId
  else "null") + '}'
}
