package org.openmole.spatialdata.utils.gis

import org.locationtech.jts.geom.{Geometry, GeometryFactory, LineString, MultiLineString}
import org.openmole.spatialdata.vector._


object GeometryUtils {

  /**
    * Get the centroid of the convex hull of a point cloud
    * @param pi
    * @return
    */
  def convexHullCentroid(pi: Array[Point]): Point = {
    val convexHullCentroid = convexHullPoints(pi).getCentroid
     (convexHullCentroid.getX,convexHullCentroid.getY)
  }

  /**
    * Get the area of the convex hull of a set of points
    * @param pi points
    * @return
    */
  def convexHullArea(pi: Array[Point]): Double = convexHullPoints(pi).getArea

  /**
    * Get the convex hull of a set of points
    * @param pi points
    * @return
    */
  def convexHullPoints(pi: Array[Point]): Geometry = {
    val geomFactory = new GeometryFactory
    geomFactory.createMultiPoint(pi.map{case (x,y)=>geomFactory.createPoint(new org.locationtech.jts.geom.Coordinate(x,y))}).convexHull
  }

  /**
    * Convert a geometry to lines
    *  - support only MultiLineString for now, without attributes
    * @param g geometry
    * @return
    */
  def geometryToLines(g: Geometry): Lines = {
    g.getGeometryType match {
      case s if s == "MultiLineString" => {
        val n = g.getNumGeometries
        Lines((0 until n).map{i => g.asInstanceOf[MultiLineString].getGeometryN(i).asInstanceOf[LineString]},Seq.empty)
      }
      case _ => Lines.empty
    }
  }

  /*
    * Convert a GeoPackage spatialfeature Geometry to a JTS Geometry
    *
    *  -- Not needed if use gt-geopkg library instead of mil.nga.Geopackage --
    *
    * @param g geomtry
    * @return
    */
  /*def toJTSGeomtry(g: mil.nga.sf.Geometry): geom.Geometry = {
    val fact = new GeometryFactory
    g.getGeometryType match {
      case mil.nga.sf.GeometryType.POINT => {
        val p = g.asInstanceOf[mil.nga.sf.Point]
        fact.createPoint(new geom.Coordinate(p.getX,p.getY))
      }
      case mil.nga.sf.GeometryType.MULTIPOINT => {
        val p = g.asInstanceOf[mil.nga.sf.MultiPoint]
        val coords = p.getPoints.asScala.toArray.map(p => new geom.Coordinate(p.getX,p.getY))
        fact.createMultiPointFromCoords(coords)
      }
      case mil.nga.sf.GeometryType.LINESTRING => {
        val l = g.asInstanceOf[mil.nga.sf.LineString]
        fact.createLineString(l.getPoints.asScala.toArray.map(p => new geom.Coordinate(p.getX,p.getY)))
      }
      case mil.nga.sf.GeometryType.MULTILINESTRING => {
        val linestrings = g.asInstanceOf[mil.nga.sf.MultiLineString]
        fact.createMultiLineString(linestrings.getLineStrings.asScala.toArray.map(
          l => fact.createLineString(l.getPoints.asScala.toArray.map(p => new geom.Coordinate(p.getX,p.getY)))
        ))
      }
      case mil.nga.sf.GeometryType.POLYGON => {
        val p = g.asInstanceOf[mil.nga.sf.Polygon]
        fact.createPolygon(p.getExteriorRing.getPoints.asScala.toArray.map(p => new geom.Coordinate(p.getX,p.getY)))
      }
      case mil.nga.sf.GeometryType.MULTIPOLYGON => {
        val polygons = g.asInstanceOf[mil.nga.sf.MultiPolygon]
        fact.createMultiPolygon(polygons.getPolygons.asScala.toArray.map{p =>
          fact.createPolygon(p.getExteriorRing.getPoints.asScala.toArray.map(p => new geom.Coordinate(p.getX,p.getY)))
        })
      }
    }
  }
  */

}
