package se.kodapan.osm.jts

import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence
import se.kodapan.osm.domain._
import java._

import scala.util.control.Breaks


/**
  * Creates JTS geometries out of OSM nodes, ways and relations.
  *
  * @author kalle
  * @since 2015-06-03 03:22
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
      val node = nodes.get(i)
      coordinates(i) = new Coordinate(node.getX, node.getY)

      {
        i += 1; i - 1
      }
    }
    if (!way.isPolygon) geometryFactory.createLineString(coordinates)
    else throw new RuntimeException("Way expected not to be a polygon.")
  }

  def createPolygon(way: Way) = {
    val coordinates = new Array[Coordinate](way.getNodes.size)
    val nodes = way.getNodes
    var i = 0
    while ( {
      i < nodes.size
    }) {
      val node = nodes.get(i)
      coordinates(i) = new Coordinate(node.getX, node.getY)

      {
        i += 1; i - 1
      }
    }
    if (!way.isPolygon) throw new RuntimeException("Way expected to be a polygon.")
    else geometryFactory.createPolygon(geometryFactory.createLinearRing(coordinates), null)
  }

  /**
    * Asserts that relation members are all outer ways that form a single polygon.
    *
    * @param relation
    * @return
    */
  def createOuterWaysPolygon(relation: Relation) = {
    val lines = new java.util.ArrayList[java.util.List[Coordinate]](relation.getMembers.size)
    import scala.collection.JavaConversions._
    for (member <- relation.getMembers) {
      if (!"outer".equalsIgnoreCase(member.getRole)) throw new RuntimeException
      val way = member.getObject.asInstanceOf[Way]
      val line = new java.util.ArrayList[Coordinate](way.getNodes.size)
      import scala.collection.JavaConversions._
      for (node <- way.getNodes) {
        line.add(new Coordinate(node.getX, node.getY))
      }
      lines.add(line)
    }
    val maxIterations = lines.size
    val sorted = new java.util.ArrayList[java.util.List[Coordinate]](lines.size)
    sorted.add(lines.remove(0))
    var iterations = 0
    while ( {
      !lines.isEmpty
    }) {
      if ( {
        iterations += 1; iterations - 1
      } >= maxIterations) throw new RuntimeException("Eternal loop")
      val lineIterator = lines.iterator
      while ( {
        lineIterator.hasNext
      }) {
        val line = lineIterator.next
        var stop = false
        import scala.collection.JavaConversions._
        val loop = new Breaks;
        loop.breakable {
          for (testLine <- new java.util.ArrayList[java.util.List[Coordinate]](sorted)) {
            if (testLine.get(testLine.size - 1) == line.get(0)) {
              sorted.add(line)
              lineIterator.remove()
              loop.break //todo: break is not supported
            }
            else if (testLine.get(testLine.size - 1) == line.get(line.size - 1)) {
              java.util.Collections.reverse(line)
              sorted.add(line)
              lineIterator.remove()
              loop.break //todo: break is not supported
            }
          }
        }
      }
    }
    var coordinatesCount = 0
    import scala.collection.JavaConversions._
    for (line <- sorted) {
      coordinatesCount += line.size
    }
    var position = 0
    val coordinates = new Array[Coordinate](coordinatesCount)
    import scala.collection.JavaConversions._
    for (line <- sorted) {
      import scala.collection.JavaConversions._
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
    val nodes = new java.util.ArrayList[Node]
    var firstNode: Node = null
    import scala.collection.JavaConversions._
    for (membership <- relation.getMembers) {
      Breaks.breakable {
        if (!"outer".equalsIgnoreCase(membership.getRole)) Breaks.break //todo: continue is not supported// todo inner as holes!
        if (firstNode == null) {
          firstNode = membership.getObject.accept(new OsmObjectVisitor[Node]() {
            override def visit(node: Node) = node
            override def visit(way: Way) = way.getNodes.get(0)
            override def visit(relation: Relation) = relation.accept(this)
          })
        }
        val nextNodes = membership.getObject.accept(new NodesCollector)
        if (nodes.isEmpty) nodes.addAll(nextNodes)
        else {
          val previousNode = nodes.get(nodes.size - 1)
          if (nextNodes.get(0) == previousNode) nodes.addAll(nextNodes)
          else if (nextNodes.get(nextNodes.size - 1) == previousNode) {
            java.util.Collections.reverse(nextNodes)
            nodes.addAll(nextNodes)
          }
          else {
            System.out.println("previous" + previousNode)
            System.out.println("first" + nextNodes.get(0))
            System.out.println("last" + nextNodes.get(nextNodes.size - 1))
            throw new RuntimeException("Non connected members in relation")
          }
        }
        if (nodes.get(nodes.size - 1) == firstNode) {
          val coordinates = new Array[Coordinate](nodes.size + 1)
          var i = 0
          while ( {
            i < nodes.size
          }) {
            val node = nodes.get(i)
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

  private val linesComparator = new java.util.Comparator[java.util.List[Node]]() {
    override def compare(l1: java.util.List[Node], l2: java.util.List[Node]) = if (coordinateEquals(l1.get(0), l2.get(l2.size - 1))) -1
    else if (coordinateEquals(l1.get(l1.size - 1), l2.get(0))) 1
    else 0

    private

    def coordinateEquals(n1: Node, n2: Node) = n1.getLatitude == n2.getLatitude && n1.getLongitude == n2.getLongitude
  }

  private class NodesCollector extends OsmObjectVisitor[java.util.List[Node]] {
    override def visit(node: Node) = {
      val nodes = new java.util.ArrayList[Node](1)
      nodes.add(node)
      nodes
    }

    override def visit(way: Way) = way.getNodes

    override def visit(relation: Relation) = {
      val lines = new java.util.ArrayList[java.util.List[Node]]
      import scala.collection.JavaConversions._
      for (membership <- relation.getMembers) {
        lines.add(membership.getObject.accept(new NodesCollector))
      }
      java.util.Collections.sort(lines, linesComparator)
      var nodesCount = 0
      import scala.collection.JavaConversions._
      for (line <- lines) {
        nodesCount += line.size
      }
      val nodes = new java.util.ArrayList[Node](nodesCount)
      import scala.collection.JavaConversions._
      for (line <- lines) {
        nodes.addAll(line)
      }
      nodes
    }
  }
}
