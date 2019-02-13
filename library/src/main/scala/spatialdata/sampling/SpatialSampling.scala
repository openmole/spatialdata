
package spatialdata.sampling

import spatialdata.Coordinate
import spatialdata.utils.io.Shapefile
import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.triangulate.ConformingDelaunayTriangulationBuilder

import scala.collection.mutable.ArrayBuffer
import scala.util.{Random, Try}

object SpatialSampling {


  /**
    *
    * @param layer
    * @param nPoints
    * @param weightAttribute
    * @return
    */
  def samplePointsInLayer(layer: String,nPoints: Int,weightAttribute: String = "")(implicit rng: Random): Seq[Coordinate] = {
    val polygons = weightAttribute match {
      case s if s.length > 0 => Shapefile.readGeometry(layer,Array(weightAttribute))
      case _ => Shapefile.readGeometry(layer).map{case (g,_)=>(g,Array(1.0))}
    }
    val attrs = polygons.map{_._2(0)}
    val s = attrs.sum
    val weights = attrs.map{_/s}

    val pointTries: Seq[Try[Coordinate]] = for{_ <- 1 to nPoints } yield {
      val r = rng.nextDouble()
      var ss=0.0
      // Delaunay triangulation sometimes fails when enforcing constraints (too fine geometries ?) => wrap in Try
      Try[Coordinate]{PolygonSampler(polygons(weights.map{case w => {ss = ss + w ; ss > r} }.zipWithIndex.filter{_._1}.head._2)._1.asInstanceOf[MultiPolygon]).
        sample}
    }
    pointTries.flatMap(_.toOption)
  }


  case class PolygonSampler(val polygon: MultiPolygon, val tolerance: Double = 0.1) {
    lazy val triangles = {
      val builder = new ConformingDelaunayTriangulationBuilder
      builder.setSites(polygon)
      builder.setConstraints(polygon)
      builder.setTolerance(tolerance)
      val triangleCollection = builder.getTriangles(polygon.getFactory).asInstanceOf[GeometryCollection]
      var areaSum = 0.0
      val trianglesInPolygon = (0 until triangleCollection.getNumGeometries).map(triangleCollection.getGeometryN(_).asInstanceOf[Polygon]).filter(p => {
        val area = p.getArea
        p.intersection(polygon).getArea > 0.99 * area
      })
      trianglesInPolygon.map { triangle =>
        areaSum += triangle.getArea
        (areaSum, triangle)
      }
    }

    lazy val totalArea = triangles.last._1

    def sample(implicit rng: Random): (Double,Double) = {
      val s = rng.nextDouble() * totalArea
      val t = rng.nextDouble()
      val triangleIndex = triangles.indexWhere(s < _._1)
      val area = triangles(triangleIndex)._1
      val previousArea = if (triangles.isDefinedAt(triangleIndex - 1)) triangles(triangleIndex - 1)._1 else 0.0
      val triangle = triangles(triangleIndex)._2
      val tmp = Math.sqrt((s - previousArea) / (area - previousArea))
      val a = 1 - tmp
      val b = (1 - t) * tmp
      val c = t * tmp
      val coord = triangle.getCoordinates
      val p1 = coord(0)
      val p2 = coord(1)
      val p3 = coord(2)
      val x1 = p1.x
      val x2 = p2.x
      val x3 = p3.x
      val y1 = p1.y
      val y2 = p2.y
      val y3 = p3.y
      val x = a * x1 + b * x2 + c * x3
      val y = a * y1 + b * y2 + c * y3
      (x,y)
    }
  }



}
