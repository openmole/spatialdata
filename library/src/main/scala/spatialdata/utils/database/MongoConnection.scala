
package spatialdata.utils.database

import java.util

import com.mongodb.MongoClient
import com.mongodb.client.{FindIterable, MongoCollection, MongoDatabase}
import com.mongodb.client.model.Filters
import com.mongodb.client.model.Filters._
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory, Polygon}
import com.vividsolutions.jts.io.WKTReader
import org.bson.Document
import org.bson.conversions.Bson

import scala.collection.mutable.ArrayBuffer



object MongoConnection {

  var mongoClient: MongoClient = null

  var mongoDatabase: MongoDatabase = null


  /**
    *
    * @param database
    * @param host
    * @param port
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
    * @param lonmin
    * @param latmin
    * @param lonmax
    * @param latmax
    * @param collection
    * @return
    */
  def bboxRequest(lonmin: Double,latmin: Double,lonmax: Double,latmax: Double,collection: String): Seq[Polygon] = {
    val mongoCollection: MongoCollection[Document] = mongoDatabase.getCollection(collection)
    val filt: util.LinkedList[Bson] = new util.LinkedList[Bson]()
    filt.add(Filters.geoWithinBox("geometry",lonmin,latmin,lonmax,latmax))
    filt.add(Filters.eq("geometry.type","LineString"))
    val queryres = mongoCollection.find(and(filt))

    val res = new ArrayBuffer[Polygon]
    val geomfact = new GeometryFactory

    while (queryres.iterator.hasNext){
      val currentdoc: Document = queryres.iterator.next
      val coords: util.List[util.LinkedList[Double]] = currentdoc.getList("geometry.coordinates",new util.LinkedList[Double]().getClass)
      val coordarray = coords.toArray.map{case l: util.LinkedList[Double] => new Coordinate(l.getFirst,l.getLast)}
      res.append(geomfact.createPolygon(geomfact.createLinearRing(coordarray)))
    }
    res
  }

  def closeMongo(): Unit = mongoClient.close()


}