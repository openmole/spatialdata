package org.openmole.spatialdata.utils.io

import java.io.{BufferedReader, File, FileReader, InputStream}

import org.apache.commons.io.IOUtils
import org.openmole.spatialdata.utils.math.{Matrix, RealMatrix}


object Binary {

  def byteArrayToFloat(x: Array[Byte]): Float = {
    var res = 0
    for (i <- 0 to 3) {
      res += ((x(i) & 0xff) << ((3 - i) * 8))
    }
    java.lang.Float.intBitsToFloat(res)
  }

  def readBinaryMatrix(file: String): Matrix = {
    val reader = new FileReader(new File(file))
    val data: Array[Byte] = IOUtils.toByteArray(reader)
    // array begins with Int - 4 bytes, rows, columns
    //val n: Int = data(0) | data(1) << 8 | data(2) << 16 | data(3) << 24
    //val p: Int = data(4) | data(5) << 8 | data(6) << 16 | data(7) << 24
    val n: Int = data(3) | data(2) << 8 | data(1) << 16 | data(0) << 24
    val p: Int = data(7) | data(6) << 8 | data(5) << 16 | data(4) << 24
    println("n="+n+",p="+p+",size="+data.length)
    //if(data.length!=n*p*4) throw new RuntimeException("Binary float matrix data is malformed")
    // sliding row by row
    RealMatrix(data.drop(8).grouped(p*4).map(_.grouped(4).map(byteArrayToFloat(_).toDouble).toArray).toArray)
  }


}
