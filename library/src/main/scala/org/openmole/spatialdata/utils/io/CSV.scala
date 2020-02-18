
package org.openmole.spatialdata.utils.io

import java.io.{BufferedReader, File, FileReader}

import com.github.tototoshi.csv._
import org.openmole.spatialdata.utils.math.{SparseMatrix, SparseMatrixImpl}

import scala.collection.mutable.ArrayBuffer


object CSV {

  /**
    * read csv file
    * @param file
    * @param sep
    * @return
    */
  def readCSVFile(file: File,sep: String=","): Map[String,Seq[String]] = {

    implicit val readerFormat = new DefaultCSVFormat {
      override val delimiter = sep.charAt(0)
      override val quoteChar: Char = '"'
    }
    val (header,content) = CSVReader.open(file)(readerFormat).allWithOrderedHeaders()
    //CSVReader.open(file)(readerFormat).all()
    header.map{case s => (s,content.map{_.get(s).get})}.toMap
  }

  def readCSV(filename: String,sep: String = ","): Map[String,Seq[String]] = readCSVFile(new File(filename),sep)

  /**
    * read a numerical matrix
    * @param file
    * @param sep
    * @return
    */
  def readMat(file: String,sep: String=",",naformat: String = "NA"): Array[Array[Double]] = {
    val r = new BufferedReader(new FileReader(new File(file)))
    val res = new ArrayBuffer[Array[Double]]
    var currentline = r.readLine()
    while(currentline!=null){
      res.append(currentline.split(sep).map{s => if(s.equals("NA")) Double.NaN else s.toDouble})
      currentline = r.readLine()
    }
    res.toArray
  }

  /**
    * Read a sparse mat as a csv (i,j,v)
    * @param file
    * @param sep
    * @param naformat
    * @return
    */
  def readSparseMat(file: String,sep: String=",",naformat: String = "NA"): SparseMatrix = {
    val entries = new ArrayBuffer[(Int,Int,Double)]
    val r = new BufferedReader(new FileReader(new File(file)))
    var currentline = r.readLine()
    while(currentline!=null){
      val entry = currentline.split(sep)
      val (i,j) = (entry(0).toInt,entry(1).toInt)
      val v = if(entry(2).equals("NA")) Double.NaN else entry(2).toDouble
      entries.append((i,j,v))
      currentline = r.readLine()
    }
    val aentries = entries.toArray
    val n = aentries.map(_._1).max // FIXME this is not correct if last columns/rows are empty
    val p = aentries.map(_._2).max
    SparseMatrixImpl(aentries,n,p)
  }


  /**
    * write a csv to a file
    * @param data
    * @param file
    * @param sep
    * @param header
    * @tparam T
    */
  def writeCSV[T <: Any](data: Array[Array[T]],file:String,sep:String,header:Array[String] = Array.empty): Unit = {
    implicit val writerFormat = new DefaultCSVFormat {
      override val delimiter = sep.charAt(0)
      override val quoteChar: Char = '"'
    }

    val towrite = if(header.size == 0){data.map{_.map{_.toString}.toSeq}.toSeq} else {Seq(header.toSeq)++data.map{_.toSeq}.toSeq}

    CSVWriter.open(file)(writerFormat).writeAll(towrite)
  }

}
