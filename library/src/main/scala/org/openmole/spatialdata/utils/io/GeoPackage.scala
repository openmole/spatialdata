package org.openmole.spatialdata.utils.io

import org.geotools.data.DataUtilities
import org.geotools.data.collection.ListFeatureCollection

import java.io.File
import org.geotools.data.simple.{SimpleFeatureCollection, SimpleFeatureReader}
import org.geotools.feature.collection.BaseFeatureCollection
import org.geotools.feature.simple.SimpleFeatureBuilder
import org.geotools.geometry.jts.GeometryCollector
import org.geotools.geopkg.FeatureEntry
import org.locationtech.jts.geom.{Geometry, GeometryCollection}
import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.vector.Attributes

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IterableHasAsJava}

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
    //utils.log("Reading geopkg: "+geopkg.features.asScala.map(_.getDescription).mkString(" ; "))
    val featureentry: FeatureEntry = if(featureName.nonEmpty) geopkg.feature(featureName) else geopkg.features().get(featureIndex)
    val freader: SimpleFeatureReader = geopkg.reader(featureentry, null, null)
    utils.log("Reading geopkg: "+freader.getFeatureType)
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


  /**
   * Write geopkg
   *   ! CRS and geom other than points not implemented yet
   *
   * @param geometry geometry
   * @param featureName feature name
   * @param file file
   */
  def writeGeometry(geometry: GeometryCollection, featureName: String, file: String): Unit = {
    val geopkg: org.geotools.geopkg.GeoPackage  = new org.geotools.geopkg.GeoPackage(new File(file))
    geopkg.init()
    val featureentry: FeatureEntry = new FeatureEntry; featureentry.setTableName(featureName)
    val featureType: SimpleFeatureType = DataUtilities.createType("Points", "the_geom:Point:srid=4326,name:String,number:Integer")
    val featureBuilder: SimpleFeatureBuilder = new SimpleFeatureBuilder(featureType)
    val features = new java.util.LinkedList[SimpleFeature]
    (0 until geometry.getNumPoints).foreach{i => featureBuilder.add(geometry.getGeometryN(i)); features.add( featureBuilder.buildFeature(null))}
    val collection: SimpleFeatureCollection = new ListFeatureCollection(featureType, features)
    geopkg.add(featureentry, collection)
    geopkg.close()
  }

}

