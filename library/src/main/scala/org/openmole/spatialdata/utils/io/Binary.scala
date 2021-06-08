package org.openmole.spatialdata.utils.io

import java.io._

import org.apache.commons.io.IOUtils
import org.nustaq.serialization.{FSTObjectInput, FSTObjectOutput}
import org.openmole.spatialdata.utils.math.{Matrix, RealMatrix}


object Binary {


  /**
    * serialize any object in binary using FST
    * @param o object
    * @param filePath file path
    */
  def writeBinary(o: AnyRef, filePath: String): Unit = {
    val out: FSTObjectOutput = new FSTObjectOutput(new FileOutputStream(filePath))
    out.writeObject(o)
    out.close()
  }


  /**
    *
    * @param filePath file path
    * @tparam T read type
    */
  def readBinary[T](filePath: String): T = {
    val in: FSTObjectInput = new FSTObjectInput(new FileInputStream(filePath))
    val result: T = in.readObject().asInstanceOf[T]
    in.close()
    result
  }




  /**
    * ! not sure of the order
    * @param x byte array
    * @return
    */
  def byteArrayToFloat(x: Array[Byte]): Float = {
    var res = 0
    for (i <- 0 to 3) {
      res += ((x(i) & 0xff) << ((3 - i) * 8))
    }
    java.lang.Float.intBitsToFloat(res)
  }

  /*
    * ! this does not work with files written by dotnet (not even the same size) - Bytes should be the same however?
    *  seems that binary formats are not necessarily compatible accross platforms
    */
   /*def readBinaryMatrix(file: String): Matrix = {
    val reader = new FileReader(new File(file))
    val data: Array[Byte] = IOUtils.toByteArray(reader)
    // or can use java.nio.files.Files.readAllBytes ?
    // array begins with Int - 4 bytes, rows, columns
    //val n: Int = data(0) | data(1) << 8 | data(2) << 16 | data(3) << 24
    //val p: Int = data(4) | data(5) << 8 | data(6) << 16 | data(7) << 24
    println(data.take(4).mkString(" "))
    val n: Int = data(3) | data(2) << 8 | data(1) << 16 | data(0) << 24
    val p: Int = data(7) | data(6) << 8 | data(5) << 16 | data(4) << 24
    println("n="+n+",p="+p+",size="+data.length)
    //if(data.length!=n*p*4) throw new RuntimeException("Binary float matrix data is malformed")
    // sliding row by row
    RealMatrix(data.drop(8).grouped(p*4).map(_.grouped(4).map(byteArrayToFloat(_).toDouble).toArray).toArray)
  }*/


}
