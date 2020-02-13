package org.openmole.spatialdata.utils.osm

import org.locationtech.jts.geom._
import org.locationtech.jts.geom.impl.CoordinateArraySequence

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.control.Breaks

/**
  * Creates JTS geometries out of OSM nodes, ways and relations.
  */
class JtsGeometryFactory(var geometryFactory: GeometryFactory = new GeometryFactory) {
  def createPoint(node: Node) = geometryFactory.createPoint(new Coordinate(node.getX, node.getY))

  def createLineString(way: Way) = {
    val coordinates = new Array[Coordinate](way.getNodes.size)
    val nodes = way.getNodes
    var i = 0
    while ( {
      i < nodes.size
    }) {
      val node = nodes(i)
      coordinates(i) = new Coordinate(node.getX, node.getY)

      {
        i += 1; i - 1
      }
    }
    if (!way.isPolygon) geometryFactory.createLineString(coordinates)
    else throw new RuntimeException("Way expected not to be a polygon.")
  }

  def createPolygon(way: Way) = {
    val coordinates = way.getNodes.map(node=>new Coordinate(node.getX, node.getY)).toArray
    if (!way.isPolygon) throw new RuntimeException("Way expected to be a polygon.")
    else geometryFactory.createPolygon(geometryFactory.createLinearRing(coordinates), null)
  }

  /**
    * Asserts that relation members are all outer ways that form a single polygon.
    *
    *  FIXME function never used
    *
    * @param relation
    * @return
    */
  def createOuterWaysPolygon(relation: Relation) = {
    val lines = new mutable.ArrayBuffer[mutable.ArrayBuffer[Coordinate]](relation.getMembers.size)
    for (member <- relation.getMembers) {
      if (!"outer".equalsIgnoreCase(member.getRole)) throw new RuntimeException
      val way = member.getObject.asInstanceOf[Way]
      val line = new mutable.ArrayBuffer[Coordinate](way.getNodes.size)
      for (node <- way.getNodes) {
        line.append(new Coordinate(node.getX, node.getY))
      }
      lines.append(line)
    }
    val maxIterations = lines.size
    val sorted = new mutable.ArrayBuffer[mutable.ArrayBuffer[Coordinate]](lines.size)
    sorted.append(lines.remove(0))
    var iterations = 0
    while (lines.nonEmpty) {
      if ( {
        iterations += 1; iterations - 1
      } >= maxIterations) throw new RuntimeException("Eternal loop")
      val lineIterator = lines.iterator
      while ( {
        lineIterator.hasNext
      }) {
        val line = lineIterator.next
        var stop = false
        val loop = new Breaks;
        loop.breakable {
          for (testLine <- sorted.toSeq) {
            if (testLine(testLine.size - 1) == line(0)) {
              sorted.append(line)
              lines.remove(lines.indexOf(line)) // FIXME check if compatible with the iterator
              loop.break //todo: break is not supported
            }
            else if (testLine(testLine.size - 1) == line(line.size - 1)) {
              sorted.append(line.reverse)
              lines.remove(lines.indexOf(line)) // FIXME
              loop.break //todo: break is not supported
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
    * @param relation
    * @return
    */
  def createMultiPolygon(relation: Relation) = {
    val linearRings = new java.util.ArrayList[LinearRing]
    val nodes = new mutable.ArrayBuffer[Node]
    var firstNode: Node = null
    for (membership <- relation.getMembers) {
      Breaks.breakable {
        if (!"outer".equalsIgnoreCase(membership.getRole)) Breaks.break //todo: continue is not supported// todo inner as holes!
        if (firstNode == null) {
          firstNode = membership.getObject.accept(new OsmObjectVisitor[Node]() {
            override def visit(node: Node): Node = node
            override def visit(way: Way): Node = way.getNodes()(0)
            override def visit(relation: Relation): Node = relation.accept(this)
          })
        }
        val nextNodes = membership.getObject.accept(new NodesCollector)
        if (nodes.isEmpty) nodes.addAll(nextNodes)
        else {
          val previousNode = nodes(nodes.size - 1)
          if (nextNodes(0) == previousNode) nodes.addAll(nextNodes)
          else if (nextNodes(nextNodes.size - 1) == previousNode) {
            //java.util.Collections.reverse(nextNodes)
            // FIXME check this replacement
            nodes.addAll(nextNodes.reverse)
          }
          else {
            System.out.println("previous" + previousNode)
            System.out.println("first" + nextNodes(0))
            System.out.println("last" + nextNodes(nextNodes.size - 1))
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
              i += 1;
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

  private class NodesCollector extends OsmObjectVisitor[mutable.ArrayBuffer[Node]] {
    override def visit(node: Node) = {
      val nodes = new mutable.ArrayBuffer[Node](1)
      nodes.append(node)
      nodes
    }

    override def visit(way: Way) = way.getNodes

    override def visit(relation: Relation) = {
      val lines = new mutable.ArrayBuffer[mutable.ArrayBuffer[Node]]
      for (membership <- relation.getMembers) {
        lines.append(membership.getObject.accept(new NodesCollector))
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
