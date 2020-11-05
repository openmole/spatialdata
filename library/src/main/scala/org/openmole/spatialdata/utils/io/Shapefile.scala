package org.openmole.spatialdata.utils.io

import java.io.{BufferedReader, File, FileReader}

import org.locationtech.jts.geom.Geometry
import org.geotools.data.shapefile.ShapefileDataStore
import org.geotools.referencing.CRS
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.vector.Attributes

import scala.collection.mutable.ArrayBuffer

object Shapefile {


  /**
    * Read a shapefile
    * @param layer layer
    * @param attributes attributes to retrieve
    * @return
    */
  def readGeometry(layer: String,attributes:Array[String]=Array.empty): Seq[(Geometry,Attributes)] = {

    val store = new ShapefileDataStore(new File(layer).toURI.toURL)
    try {
      val reader = store.getFeatureReader
      utils.log("Reading : "+reader.getFeatureType)
      try {
          //val featureReader = Iterator.continually{val next = reader.next;println(next);next}.takeWhile(_ => reader.hasNext)
          //filter(feature => filter(feature.getAttribute("attr").toString))
          val geoms = new ArrayBuffer[(Geometry,Attributes)]
          while(reader.hasNext){
            val feature = reader.next()
            geoms.append((feature.getDefaultGeometry.asInstanceOf[Geometry],attributes.map{s => (s,feature.getAttribute(s).toString.toDouble.asInstanceOf[AnyRef])}.toMap))
          }
          geoms.toSeq
        }finally reader.close()
      } finally store.dispose()
  }

  /**
    * Get projection EPSG code for a given layer
    *
    *  Rq: wkt string is assumed on first line of .prj file
    *
    * @param layer layer path
    * @return
    */
  def getLayerEPSG(layer: String): String = {
    //val fsplit = layer.split("\\.").toSeq
    //fsplit.take(fsplit.length-1).mkString(".")+".prj")
    val layerprefix = layer.substring(0,layer.length-4)
    val crs = CRS.parseWKT(new BufferedReader(new FileReader(new File(layerprefix+".prj"))).readLine())
    "EPSG:"+CRS.lookupEpsgCode(crs,true)
  }

  /**
    * Get layer CRS
    * 
    * @param layer layer path
    * @return
    */
  def getLayerCRS(layer: String): CoordinateReferenceSystem = CRS.parseWKT(new BufferedReader(new FileReader(new File(layer.substring(0,layer.length-4)+".prj"))).readLine())



}
