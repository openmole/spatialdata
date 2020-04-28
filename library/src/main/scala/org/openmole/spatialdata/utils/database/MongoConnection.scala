package org.openmole.spatialdata.utils.database

import java.util

import com.mongodb.MongoClient
import com.mongodb.client.model.Filters
import com.mongodb.client.model.Filters.and
import com.mongodb.client.{MongoCollection, MongoDatabase}
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Polygon}
import org.bson.Document
import org.bson.conversions.Bson

import scala.collection.mutable.ArrayBuffer

object MongoConnection {

  var mongoClient: MongoClient = _

  var mongoDatabase: MongoDatabase = _


  /**
    *
    * @param database database
    * @param host host
    * @param port port
    */
  def initMongo(database: String,host: String = "127.0.0.1", port: Int = 27017): Unit = {
    try {
      mongoClient = new MongoClient(host,port)
      mongoDatabase = mongoClient.getDatabase(database)
    }catch {
      case e: Exception => e.printStackTrace()
    }
  }

  /**
    *
    *
    * @param lonmin min longitude
    * @param latmin min latitude
    * @param lonmax max longitude
    * @param latmax max latitude
    * @param collection collection
    * @return
    */
  def bboxRequest(lonmin: Double,latmin: Double,lonmax: Double,latmax: Double,collection: String,limit: Int = -1): Seq[Polygon] = {
    val mongoCollection: MongoCollection[Document] = mongoDatabase.getCollection(collection)
    val filt: util.LinkedList[Bson] = new util.LinkedList[Bson]()
    filt.add(Filters.geoWithinBox("geometry.coordinates",lonmin,latmin,lonmax,latmax))
    filt.add(Filters.eq("geometry.type","LineString"))
    val queryres = mongoCollection.find(and(filt))

    val res = new ArrayBuffer[Polygon]
    val geomfact = new GeometryFactory

    while (queryres.iterator.hasNext&&(limit<0||res.length<limit)){
      val currentdoc: Document = queryres.iterator.next
      val coords = currentdoc.get("geometry").asInstanceOf[Document].get("coordinates").asInstanceOf[util.ArrayList[AnyRef]].toArray.map{l  => new Coordinate(l.asInstanceOf[util.ArrayList[Double]].get(0),l.asInstanceOf[util.ArrayList[Double]].get(1))}
      res.append(geomfact.createPolygon(geomfact.createLinearRing(coords)))
    }
    res.toSeq
  }

  def closeMongo(): Unit = mongoClient.close()


}
