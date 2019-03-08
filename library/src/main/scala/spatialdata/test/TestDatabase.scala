
package spatialdata.test


import spatialdata.utils.database._

object TestDatabase {



  def testPostgis(): Unit = {
    PostgisConnection.initPostgis("testbuildings")
    val polygons = PostgisConnection.bboxRequest(1,40,2,45,"ways")
    println(polygons)
    println(polygons.length)
  }

}





