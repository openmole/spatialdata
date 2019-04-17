
package org.openmole.spatialdata.utils.io

import java.io.{BufferedReader, File, FileReader}

import com.github.tototoshi.csv._

import scala.collection.mutable.ArrayBuffer


object CSV {

  /**
    * read csv file
    * @param file
    * @param sep
    * @return
    */
  def readCSV(file: String,sep: String=","): Map[String,Seq[String]] = {

    implicit val readerFormat = new DefaultCSVFormat {
      override val delimiter = sep.charAt(0)
      override val quoteChar: Char = '"'
    }
    val (header,content) = CSVReader.open(file)(readerFormat).allWithOrderedHeaders()
    //CSVReader.open(file)(readerFormat).all()
    header.map{case s => (s,content.map{_.get(s).get})}.toMap
  }

  /**
    * read a numerical matrix
    * @param file
    * @param sep
    * @return
    */
  def readMat(file: String,sep: String=","): Array[Array[Double]] = {
    val r = new BufferedReader(new FileReader(new File(file)))
    val res = new ArrayBuffer[Array[Double]]
    var currentline = r.readLine()
    while(currentline!=null){
      res.append(currentline.split(sep).map{_.toDouble})
      currentline = r.readLine()
    }
    res.toArray
  }

  def writeCSV(data: Array[Array[Double]],file:String,sep:String,header:Array[String] = Array.empty) = {
    implicit val writerFormat = new DefaultCSVFormat {
      override val delimiter = sep.charAt(0)
      override val quoteChar: Char = '"'
    }

    val towrite = if(header.size == 0){data.map{_.map{_.toString}.toSeq}.toSeq} else {Seq(header.toSeq)++data.map{_.toSeq}.toSeq}

    CSVWriter.open(file)(writerFormat).writeAll(towrite)
  }

}
