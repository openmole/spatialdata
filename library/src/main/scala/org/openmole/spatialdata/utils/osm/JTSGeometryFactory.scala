package org.openmole.spatialdata.utils.osm

import org.locationtech.jts.geom._
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.openmole.spatialdata.utils

import scala.collection.mutable
import scala.util.control.Breaks
import org.openmole.spatialdata.utils.osm.OSMObject._

/**
  * Creates JTS geometries out of OSM nodes, ways and relations.
  */
class JTSGeometryFactory(var geometryFactory: GeometryFactory = new GeometryFactory) {
  def createPoint(node: Node): Point = geometryFactory.createPoint(new Coordinate(node.getX, node.getY))

  def createLineString(way: Way): LineString = {
    geometryFactory.createLineString(way.nodes.map(n => new Coordinate(n.getX, n.getY)).toArray)
    //else throw new RuntimeException("Way expected not to be a polygon.") can have closed linestrings
  }

  def createPolygon(way: Way): Polygon = {
    val coordinates = way.nodes.map(node=>new Coordinate(node.getX, node.getY)).toArray
    //if (!way.isPolygon) throw new RuntimeException("Way expected to be a polygon.")
    //else geometryFactory.createPolygon(geometryFactory.createLinearRing(coordinates), null)
    geometryFactory.createPolygon(geometryFactory.createLinearRing(if (way.isPolygon) coordinates else coordinates++Array(coordinates.last)), null)
  }

  /**
    * Asserts that relation members are all outer ways that form a single polygon.
    *
    *  ! function never used
    *
    * @param relation relation
    * @return
    */
  def createOuterWaysPolygon(relation: Relation): Polygon = {
    val lines = new mutable.ArrayBuffer[mutable.ArrayBuffer[Coordinate]](relation.members.size)
    for (member <- relation.members) {
      if (!"outer".equalsIgnoreCase(member.getRole)) throw new RuntimeException
      val way = member.getOsmObject().asInstanceOf[Way]
      val line = new mutable.ArrayBuffer[Coordinate](way.nodes.size)
      for (node <- way.nodes) {
        line.append(new Coordinate(node.getX, node.getY))
      }
      lines.append(line)
    }
    val maxIterations = lines.size
    val sorted = new mutable.ArrayBuffer[mutable.ArrayBuffer[Coordinate]](lines.size)
    sorted.append(lines.remove(0))
    var iterations = 0
    while (lines.nonEmpty) {
      iterations += 1
      if ( (iterations - 1) >= maxIterations) throw new RuntimeException("Eternal loop")
      val lineIterator = lines.iterator
      while (lineIterator.hasNext) {
        val line = lineIterator.next()
        val loop = new Breaks
        loop.breakable {
          for (testLine <- sorted.toSeq) {
            if (testLine(testLine.size - 1) == line(0)) {
              sorted.append(line)
              lines.remove(lines.indexOf(line)) // ! check if compatible with the iterator
              loop.break()
            }
            else if (testLine(testLine.size - 1) == line(line.size - 1)) {
              sorted.append(line.reverse)
              lines.remove(lines.indexOf(line))
              loop.break()
            }
          }
        }
      }
    }
    var coordinatesCount = 0
    for (line <- sorted) {
      coordinatesCount += line.size
    }
    var position = 0
    val coordinates = new Array[Coordinate](coordinatesCount)
    for (line <- sorted) {
      for (coordinate <- line) {
        coordinates({
          position += 1; position - 1
        }) = coordinate
      }
    }
    geometryFactory.createPolygon(geometryFactory.createLinearRing(coordinates), null)
  }

  /**
    * Asserts that relation members are in order and the same direction.
    * Does not support polygons with holes (role=inner)
    *
    * @param relation relation
    * @return
    */
  def createMultiPolygon(relation: Relation): MultiPolygon = {
    val linearRings = new java.util.ArrayList[LinearRing]
    val nodes = new mutable.ArrayBuffer[Node]
    var firstNode: Node = null
    for (membership <- relation.members) {
      Breaks.breakable {
        if (!"outer".equalsIgnoreCase(membership.getRole)) Breaks.break()
        // ! inner as holes!
        if (firstNode == null) {
          firstNode = membership.getOsmObject().accept(new OSMObjectVisitor[Node]() {
            override def visit(node: Node): Node = node
            override def visit(way: Way): Node = way.nodes(0)
            override def visit(relation: Relation): Node = relation.accept(this)
          })
        }
        val nextNodes = membership.getOsmObject().accept(new NodesCollector)
        if (nodes.isEmpty) nodes.addAll(nextNodes)
        else {
          val previousNode = nodes(nodes.size - 1)
          if (nextNodes(0) == previousNode) nodes.addAll(nextNodes)
          else if (nextNodes(nextNodes.size - 1) == previousNode) {
            //java.util.Collections.reverse(nextNodes)
            // ! check this replacement
            nodes.addAll(nextNodes.reverse)
          }
          else {
            utils.log("previous" + previousNode)
            utils.log("first" + nextNodes(0))
            utils.log("last" + nextNodes(nextNodes.size - 1))
            throw new RuntimeException("Non connected members in relation")
          }
        }
        if (nodes(nodes.size - 1) == firstNode) {
          val coordinates = new Array[Coordinate](nodes.size + 1)
          var i = 0
          while ( {
            i < nodes.size
          }) {
            val node = nodes(i)
            coordinates(i) = new Coordinate(node.getX, node.getY)

            {
              i += 1
              i - 1
            }
          }
          coordinates(coordinates.length - 1) = coordinates(0)
          linearRings.add(new LinearRing(new CoordinateArraySequence(coordinates), geometryFactory))
          firstNode = null
          nodes.clear()
        }
      }
    }
    val polygons = new Array[Polygon](linearRings.size)
    var i = 0
    while ( {
      i < linearRings.size
    }) {
      polygons(i) = new Polygon(linearRings.get(i), null, geometryFactory)

      {
        i += 1; i - 1
      }
    }
    geometryFactory.createMultiPolygon(polygons)
  }

  /*
  private val linesComparator = new java.util.Comparator[java.util.List[Node]]() {
    override def compare(l1: java.util.List[Node], l2: java.util.List[Node]) = if (coordinateEquals(l1.get(0), l2.get(l2.size - 1))) -1
    else if (coordinateEquals(l1.get(l1.size - 1), l2.get(0))) 1
    else 0
  }*/

  private def coordinateEquals(n1: Node, n2: Node) = n1.getLatitude == n2.getLatitude && n1.getLongitude == n2.getLongitude

  private def compareLines(l1: mutable.ArrayBuffer[Node], l2: mutable.ArrayBuffer[Node]): Boolean =
    if (coordinateEquals(l1(0), l2(l2.size - 1))) true
    else if (coordinateEquals(l1(l1.size - 1), l2(0))) false
    else true

  private class NodesCollector extends OSMObjectVisitor[mutable.ArrayBuffer[Node]] {
    override def visit(node: Node): mutable.ArrayBuffer[Node] = {
      val nodes = new mutable.ArrayBuffer[Node](1)
      nodes.append(node)
      nodes
    }

    override def visit(way: Way): mutable.ArrayBuffer[Node] = way.nodes

    override def visit(relation: Relation): mutable.ArrayBuffer[Node] = {
      val lines = new mutable.ArrayBuffer[mutable.ArrayBuffer[Node]]
      for (membership <- relation.members) {
        lines.append(membership.getOsmObject().accept(new NodesCollector))
      }
      lines.sortInPlaceWith(compareLines)
      var nodesCount = 0
      for (line <- lines) {
        nodesCount += line.size
      }
      val nodes = new mutable.ArrayBuffer[Node](nodesCount)
      for (line <- lines) {
        nodes.addAll(line)
      }
      nodes
    }
  }
}
