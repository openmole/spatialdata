
package spatialdata.utils.database

import com.mongodb.MongoClient
import com.mongodb.client.MongoDatabase
import com.vividsolutions.jts.geom.Polygon



object MongoConnection {

  var mongoClient: MongoClient = null

  var mongoDatabase: MongoDatabase = null


  def initMongo(database: String,host: String = "127.0.0.1", port: Int = 27017): Unit = {
    try {
      mongoClient = new MongoClient(host,port)
      mongoDatabase = mongoClient.getDatabase(database)
    }catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def bboxRequest(lonmin: Double,latmin: Double,lonmax: Double,latmax: Double,table: String): Seq[Polygon] = Seq.empty


}