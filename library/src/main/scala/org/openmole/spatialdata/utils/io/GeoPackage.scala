package org.openmole.spatialdata.utils.io

import java.io.File

import mil.nga.geopackage.features.user._
import mil.nga.geopackage.geom.GeoPackageGeometryData
import mil.nga.geopackage.{GeoPackage, GeoPackageManager}
import org.locationtech.jts.geom.Geometry
import org.openmole.spatialdata.utils.gis.GeometryUtils
import org.openmole.spatialdata.vector.Attributes

import scala.collection.mutable.ArrayBuffer

object GeoPackage {


  /**
    * Read features from a gpkg file
    * GeoPackage specification: https://www.geopackage.org/
    *
    * alternative lib: https://mvnrepository.com/artifact/org.geotools/gt-geopkg/22.2 (less painful for geom conversion?)
    *
    * @param layer layer path
    * @param attributes attributes to be retrieved
    * @return
    */
  def readGeometry(layer: String, featureIndex: Int = 0, featureName: String = "", attributes: Array[String]=Array.empty): Seq[(Geometry,Attributes)] = {
    val geoPackage: GeoPackage = GeoPackageManager.open(new File(layer))
    val features = geoPackage.getFeatureTables
    val featureDao: FeatureDao  = geoPackage.getFeatureDao(if(featureName.length>0) featureName else features.get(featureIndex))
    val featureResultSet: FeatureResultSet = featureDao.queryForAll()
    val res: ArrayBuffer[(Geometry,Attributes)] = new ArrayBuffer
    try {
      while (featureResultSet.moveToNext()) {
        val featureRow: FeatureRow  = featureResultSet.getRow
        val geometryData: GeoPackageGeometryData = featureRow.getGeometry
        if (geometryData != null && !geometryData.isEmpty) {
          val geometry: Geometry = GeometryUtils.toJTSGeomtry(geometryData.getGeometry)
          res.append((geometry,attributes.map{s => (s,featureRow.getValue(s))}.toMap))
        }
      }
    } finally {
      featureResultSet.close()
    }
    geoPackage.close()
    res.toSeq
  }

}

