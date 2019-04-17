package org.openmole.spatialdata.utils.osm.domain

import java.io.Serializable
import java.util.{ArrayList, List}

import org.openmole.spatialdata.utils.osm.domain.root.NotLoadedException

import scala.beans.BeanProperty


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
