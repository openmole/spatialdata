package se.kodapan.osm.domain

import java.io.Serializable


/**
  * @author kalle
  * @since 2013-05-01 15:42
  */
@SerialVersionUID(1l)
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
