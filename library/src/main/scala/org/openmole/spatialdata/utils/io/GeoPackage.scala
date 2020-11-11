package org.openmole.spatialdata.utils.io

import java.io.File

import org.geotools.data.simple.SimpleFeatureReader
import org.geotools.geopkg.FeatureEntry
import org.locationtech.jts.geom.Geometry
import org.opengis.feature.simple.SimpleFeature
import org.openmole.spatialdata.vector.Attributes

import scala.collection.mutable.ArrayBuffer

/**
  * Primitives to handle GeoPackage data
  */
object GeoPackage {


  /**
    * Read features from a gpkg file
    * GeoPackage specification: https://www.geopackage.org/
    *
    * gt-geopkg lib: https://mvnrepository.com/artifact/org.geotools/gt-geopkg/22.2 (less painful for geom conversion)
    *
    * Alternative lib (less dependancies?):   Geopackage-java library: https://github.com/ngageoint/geopackage-java
    *
    * @param layer layer path
    * @param featureIndex used if featureName is not provided, index of the feature collection to be read
    * @param featureName name of the feature collection to be read
    * @param attributes attributes to be retrieved
    * @param geometryColumn name of the column containing the geometry (default "geom")
    * @return
    */
  def readGeometry(layer: String, featureIndex: Int = 0, featureName: String = "", attributes: Array[String]=Array.empty, geometryColumn: String = "geom"): Seq[(Geometry,Attributes)] = {
    val geopkg: org.geotools.geopkg.GeoPackage  = new org.geotools.geopkg.GeoPackage(new File(layer))
    val featureentry: FeatureEntry = if(featureName.length>0) geopkg.feature(featureName) else geopkg.features().get(featureIndex)
    val freader: SimpleFeatureReader = geopkg.reader(featureentry, null, null)
    val res: ArrayBuffer[(Geometry,Attributes)] = new ArrayBuffer
    while (freader.hasNext) {
      val feature: SimpleFeature = freader.next()
      val g: Geometry = feature.getAttribute(geometryColumn).asInstanceOf[Geometry]
      if (g != null && !g.isEmpty) res.append((g,attributes.map{s => (s,feature.getAttribute(s))}.toMap))
    }
    freader.close()
    geopkg.close()
    res.toSeq
  }

}

