package org.openmole.spatialdata.test

import org.openmole.spatialdata.utils.database.{MongoConnection, PostgisConnection}

object TestDatabase {


  def testMongo(): Unit = {
    MongoConnection.initMongo("testbuildings")
    val polygons = MongoConnection.bboxRequest(1,40,2,45,"buildings",5)
    println(polygons)
    println(polygons.length)
    MongoConnection.closeMongo()
  }



  def testPostgis(): Unit = {
    PostgisConnection.initPostgis("testbuildings")
    val polygons = PostgisConnection.bboxRequest(1,40,2,45,"ways")
    println(polygons)
    println(polygons.length)
  }

}
