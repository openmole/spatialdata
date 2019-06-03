
package org.openmole.spatialdata.utils.gis

import org.locationtech.jts.geom.{Coordinate, CoordinateSequence, CoordinateSequenceFilter, GeometryFactory}
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.opengis.referencing.crs.CoordinateReferenceSystem

import org.openmole.spatialdata

object GISUtils {

  val wgs84 = CRS.parseWKT("GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.01745329251994328,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]")


  /**
    * Convert WGS84 coordinates to EPSG:3857 - WGS 84 / Pseudo-Mercator -- Spherical Mercator.
    * @param lon longitude
    * @param lat latitude
    * @return Pseudo-Mercator coordinates
    */
  def WGS84ToPseudoMercator(lon: Double, lat: Double) = {
    val x = lon * 20037508.34 / 180
    val y = (Math.log(Math.tan((90 + lat) * Math.PI / 360)) / (Math.PI / 180) ) * 20037508.34 / 180
    (x, y)
  }
  /**
    * Convert EPSG:3857 - WGS 84 / Pseudo-Mercator -- Spherical Mercator coordinates to WGS84 - EPSG:4326.
    * @param x x
    * @param y y
    * @return WGS84 - EPSG:4326 (lon, lat)
    */
  def PseudoMercatorToWGS84Mercator(x: Double, y: Double) = {
    val lon = (x / 20037508.34) * 180
    val lat = (y / 20037508.34) * 180
    (lon, 180 / Math.PI * (2 * Math.atan(Math.exp(lat * Math.PI / 180)) - Math.PI / 2))
  }
  /**
    * CoordinateSequenceFilter to convert WGS84 coordinate sequences to WGS 84 / Pseudo-Mercator (EPSG:3857).
    */
  class WGS84toPseudoMercatorFilter extends CoordinateSequenceFilter {
    def filter(seq: CoordinateSequence, i: Int) = {
      if ((i != seq.size - 1) || (seq.getCoordinate(i) != seq.getCoordinate(0))) {
        val coord = seq.getCoordinate(i)
        val t = WGS84ToPseudoMercator(coord.x, coord.y)
        coord.setCoordinate(new Coordinate(t._1, t._2))
      }
    }
    def isDone = false
    def isGeometryChanged = true
  }


  /**
    * compute bounding wgs84 coordinates from lon,lat and windowSize (in meters)
    * @param lon
    * @param lat
    * @param windowSize
    * @return
    */
  def wgs84window(lon: Double,lat: Double,windowSize: Double): (Double,Double,Double,Double) = {
    val (x, y) = GISUtils.WGS84ToPseudoMercator(lon, lat)
    val (west, south) = GISUtils.PseudoMercatorToWGS84Mercator(x - windowSize / 2, y - windowSize / 2)
    val (east, north) = GISUtils.PseudoMercatorToWGS84Mercator(x + windowSize / 2, y + windowSize / 2)
    (west,south,east,north)
  }



  /**
    * Transform a set of points
    * @param points
    * @param sourceCRS
    * @param targetCRS
    * @return
    */
  def transform(points: Seq[spatialdata.Coordinate],sourceCRS: String,targetCRS: String): Seq[spatialdata.Coordinate] = {
    val source = CRS.decode(sourceCRS)
    val target = CRS.decode(targetCRS)
    val transform = CRS.findMathTransform(source, target, true)
    val geomFactory = new GeometryFactory
    points.map { p => {
      val c = JTS.transform(geomFactory.createPoint(new Coordinate(p._1, p._2)), transform).getCoordinate;
      (c.x, c.y)
      }
    }
  }

  def transform(points: Seq[spatialdata.Coordinate],source: CoordinateReferenceSystem,target: CoordinateReferenceSystem): Seq[spatialdata.Coordinate] = {

    val transform = CRS.findMathTransform(source, target, true)
    val geomFactory = new GeometryFactory
    points.map { p => {
      val c = JTS.transform(geomFactory.createPoint(new Coordinate(p._1, p._2)), transform).getCoordinate;
      (c.x, c.y)
    }
    }
  }


}

