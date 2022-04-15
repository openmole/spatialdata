package org.openmole.spatialdata.vector.real

import org.openmole.spatialdata.utils.gis.GISUtils
import org.openmole.spatialdata.utils.osm.APIExtractor
import org.openmole.spatialdata.utils.osm.APIExtractor.{OSMAPIMode, OSMOverpass}
import org.openmole.spatialdata.vector.{Attributes, Polygons, PolygonsGenerator}
import org.locationtech.jts.geom

import scala.util.Random

case class OSMBuildingsGenerator(lon: Double,
                                 lat: Double,
                                 windowSize: Double,
                                 mode: OSMAPIMode = OSMOverpass,
                                 attributes: Seq[String] = Seq.empty[String]
                                ) extends PolygonsGenerator {

  override def generatePolygons(implicit rng: Random): Polygons = {
    val (west,south,east,north)=GISUtils.wgs84window(lon,lat,windowSize)
    val (buildings, attrs)= APIExtractor.Buildings.getBuildings(south, west, north, east, mode, attributes = attributes)
    //Polygons(buildings, Seq.fill(buildings.length)(Attributes.empty))
    Polygons(buildings, attrs)
  }

}
