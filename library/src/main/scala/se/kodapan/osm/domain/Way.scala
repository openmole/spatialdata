package se.kodapan.osm.domain

import se.kodapan.osm.domain.root.NotLoadedException

import java.io.Serializable

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}

/**
  * @author kalle
  * @since 2013-05-01 15:42
  */
@SerialVersionUID(1l)
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
