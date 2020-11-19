package org.openmole.spatialdata.application.matsim

import org.locationtech.jts.geom
import org.openmole.spatialdata.utils.io.GeoPackage
import org.openmole.spatialdata.vector.Polygons

/**
  * General functions for the Matsim integration
  */
object Matsim {

  def parseArg(args: Array[String], key: String): String = {
    val filtered = args.filter(_.split("=")(0).substring(2).equals(key))
    if (filtered.isEmpty) throw new IllegalArgumentException("Required argument: --"+key)
    filtered(0).split("=")(1)
  }

  def loadArea(fuanames: Array[String], fuapath: String): geom.Geometry = {
    // load FUAs as polygons
    val fualayername = fuapath.split("/").last.split("\\.")(0)
    println("FUAs file path: "+fuapath+" ; layername: "+fualayername)
    val allfuas = GeoPackage.readGeometry(fuapath,featureName = fualayername, attributes = Array("eFUA_name","Cntry_ISO"))
    // name field for FUAs assumed as eFUA_name (JRC file) - add this as an option?
    // hardcoded for UK (anyway done with file structure)
    val fuas = allfuas.filter(f => fuanames.contains(f._2.getOrElse("eFUA_name","").asInstanceOf[String])&&f._2.getOrElse("Cntry_ISO","").asInstanceOf[String].equals("GBR"))
    //val fuas = Polygons.fromGeometries(fuasgeoms,fuasattrs) // this fails as FUAs are multipolygons
    // if several FUAs, take the bounding box to ensure a connected network, otherwise juste the polygon (take first of multipolygon)
    // (anyway mask is implemented with bbox in the GISNetworkGenerator)
     if(fuas.length==1) fuas.head._1.asInstanceOf[geom.MultiPolygon].getGeometryN(0) else Polygons(fuas.map(_._1.asInstanceOf[geom.MultiPolygon].getGeometryN(0).asInstanceOf[geom.Polygon])).getEnvelope
  }

}
