package org.openmole.spatialdata.utils.gis

import org.locationtech.jts.geom.{Geometry, GeometryCollection, GeometryFactory, MultiPolygon, Polygon}
import org.locationtech.jts.triangulate.ConformingDelaunayTriangulationBuilder
import org.openmole.spatialdata.vector.{Attributes, Point}
import org.openmole.spatialdata.utils.io.Shapefile

import scala.util.{Random, Try}



/**
  * Primitives for layer sampling
  *
  * generic caracs of a spatial sampling ? -> not necessary ; here only GIS primitives
  */
object LayerSampling {


  /**
    *
    * @param layer layer
    * @param nPoints number of points
    * @param weightAttribute weights
    * @return
    */
  def samplePointsInLayer(layer: String,nPoints: Int,weightAttribute: String = "")(implicit rng: Random): Seq[Point] = {
    val polygons: Seq[(Geometry,Double)] = weightAttribute match {
      case w if w.length > 0 => Shapefile.readGeometry(layer,Array(weightAttribute)).map{case (g,a) => (g,a(weightAttribute).asInstanceOf[Double])} // will raise exception if the weight attribute cannot be cast to Double
      case _ => Shapefile.readGeometry(layer).map{case (g,_)=>(g,1.0)}
    }
    val wvalues = polygons.map{_._2}
    val s = wvalues.sum
    val weights = wvalues.map{_/s}

    val pointTries: Seq[Try[Point]] = for{_ <- 1 to nPoints } yield {
      val r = rng.nextDouble()
      var ss=0.0
      // Delaunay triangulation sometimes fails when enforcing constraints (too fine geometries ?) => wrap in Try
      Try[Point]{PolygonSampler(polygons(weights.map{
        w =>
          ss = ss + w
          ss > r
      }.zipWithIndex.filter{_._1}.head._2)._1.asInstanceOf[MultiPolygon]).
        sample}
    }
    pointTries.flatMap(_.toOption)
  }


  case class PolygonSampler(polygon: MultiPolygon, tolerance: Double = 0.1) {
    lazy val triangles: Seq[(Double,Polygon)] = {
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

    lazy val totalArea: Double = triangles.last._1

    /**
      * ! implement simultanous multi sampling
      * @param rng rng
      * @return
      */
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

  object PolygonSampler {

    def apply(polygon: Polygon): PolygonSampler =
      PolygonSampler((new GeometryFactory).createMultiPolygon(Array(polygon)))

  }


}
