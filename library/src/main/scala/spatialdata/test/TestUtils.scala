
package spatialdata.test

import spatialdata.utils.io.CSV

object TestUtils {

  def testCSV(): Unit = {

    val res = CSV.readCSV("data/test/sample.csv",",")
    println(res)


  }

}