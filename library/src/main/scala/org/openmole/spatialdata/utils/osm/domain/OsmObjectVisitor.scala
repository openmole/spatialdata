package org.openmole.spatialdata.utils.osm.domain

/**
  * @author kalle
  * @since 2013-05-01 16:49
  */
trait OsmObjectVisitor[R] {
  def visit(node: Node): R

  def visit(way: Way): R

  def visit(relation: Relation): R
}
