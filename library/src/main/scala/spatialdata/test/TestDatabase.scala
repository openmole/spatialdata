
package spatialdata.test


import spatialdata.utils.database._

object TestDatabase {



  def testPostgis(): Unit = {
    PostgisConnection.initPostgis("testbuildings")
    println(PostgisConnection.bboxRequest(1,44,2,45,"ways"))
  }

}





