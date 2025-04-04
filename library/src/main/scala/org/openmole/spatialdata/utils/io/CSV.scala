
package org.openmole.spatialdata.utils.io

import java.io.{BufferedReader, File, FileReader}

import breeze.linalg.CSCMatrix
import com.github.tototoshi.csv._
import org.apache.commons.math3.linear
import org.openmole.spatialdata.utils.math.SparseMatrix.{SparseBreeze, SparseCommons}
import org.openmole.spatialdata.utils.math.{BreezeSparseMatrix, SparseMatrix, SparseMatrixImpl}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object CSV {

  /**
    * read csv file
    * @param file file
    * @param sep separator
    * @return
    */
  def readCSVFile(file: File,sep: String=",", withHeader: Boolean =true): Map[String,Seq[String]] = {

    implicit val readerFormat: CSVFormat = new DefaultCSVFormat {
      override val delimiter: Char = sep.charAt(0)
      override val quoteChar: Char = '"'
    }
    val (header,content): (Seq[String], Seq[Map[String, String]]) = {
      if (withHeader) CSVReader.open(file)(readerFormat).allWithOrderedHeaders()
      else {
        val raw = CSVReader.open(file)(readerFormat).all()
        val h = raw.head.indices.map("V"+_)
        (h,raw.map{row => row.zip(h).toMap})
      }
    }
    header.map(s => (s,content.map{_(s)})).toMap
  }

  /**
    * read csv (file name)
    * @param filename file
    * @param sep separator
    * @param withHeader read header
    * @return
    */
  def readCSV(filename: String, sep: String = ",", withHeader: Boolean =true): Map[String,Seq[String]] = readCSVFile(new File(filename),sep, withHeader)

  def readCSVraw(filename: String, sep: String = ",", header: Boolean = true):Array[Array[String]] = {
    val s = Source.fromFile(filename)
    val lines = if (header) s.getLines().toArray else s.getLines().toArray.tail
    val res = lines.map(_.split(sep))
    s.close()
    res
  }


  /**
    * read a numerical matrix
    * @param file file
    * @param sep separator
    * @return
    */
  def readMat(file: String,sep: String=",",naformat: String = "NA"): Array[Array[Double]] = {
    val r = new BufferedReader(new FileReader(new File(file)))
    val res = new ArrayBuffer[Array[Double]]
    var currentline = r.readLine()
    while(currentline!=null){
      res.append(currentline.split(sep).map{s => if(s.equals(naformat)) Double.NaN else s.toDouble})
      currentline = r.readLine()
    }
    res.toArray
  }

  /**
    * filter and convert a dense mat as sparse from csv
    *  for perf reasons, use directly builder for sparse breeze here => specific implementation!
    *  *spec: first line contains n,p*
    * @param file file
    * @param filter filter based on value
    * @param sep separator
    * @param naformat NA
    * @return
    */
  def readSparseMatFromDense(file: String,
                             filter: Double => Boolean,
                             convert: Double => Double,
                             sep: String=",",
                             naformat: String = "NA"
                            )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix = {
    val r = new BufferedReader(new FileReader(new File(file)))
    var currentline = r.readLine()
    val rawdims = currentline.split(sep)
    val (n,p) = (rawdims(0).toInt,rawdims(1).toInt)

    spMatImpl match {
      case _: SparseBreeze =>
        val breezebuilder = new CSCMatrix.Builder[Double](rows = n, cols = p)
        currentline = r.readLine()
        var i = 0
        while (currentline != null) {
          currentline.split(sep).zipWithIndex.map { case (s, j) =>
            if (s.equals("NA")) None else {
              if (filter(s.toDouble)) Some((i, j, convert(s.toDouble))) else None
            }
          }.filter(_.isDefined).foreach { case Some((row, col, v)) => breezebuilder.add(row, col, v); case None => () }
          currentline = r.readLine()
          i = i + 1
        }
        BreezeSparseMatrix(breezebuilder.result())

      case _: SparseCommons =>
        val m:linear.OpenMapRealMatrix = new linear.OpenMapRealMatrix(n,p)
        currentline = r.readLine()
        var i = 0
        while (currentline != null) {
          currentline.split(sep).zipWithIndex.map { case (s, j) =>
            if (s.equals("NA")) None else {
              if (filter(s.toDouble)) Some((i, j, convert(s.toDouble))) else None
            }
          }.filter(_.isDefined).foreach { case Some((row, col, v)) => m.setEntry(row, col, v); case None => () }
          currentline = r.readLine()
          i = i + 1
        }
        SparseMatrixImpl(m)

    }


  }

  /**
    * Read a sparse mat as a csv (i,j,v)
    *  **Spec: first line is (N,P)**
    * @param file file
    * @param sep separator
    * @param naformat NA
    * @return
    */
  def readSparseMat(file: String,
                    sep: String=",",
                    naformat: String = "NA"
                   )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix = {
    val entries = new ArrayBuffer[(Int,Int,Double)]
    val r = new BufferedReader(new FileReader(new File(file)))
    var currentline = r.readLine()
    val entry = currentline.split(sep)
    val (n,p) = (entry(0).toInt,entry(1).toInt)
    currentline = r.readLine()
    while(currentline!=null){
      val entry = currentline.split(sep)
      val (i,j) = (entry(0).toInt,entry(1).toInt)
      val v = if(entry(2).equals("NA")) Double.NaN else entry(2).toDouble
      entries.append((i,j,v))
      currentline = r.readLine()
    }
    val aentries = entries.toArray
    SparseMatrix(aentries,n,p)
  }


  /**
    * write a csv to a file
    * @param data data
    * @param file file
    * @param sep separator
    * @param header header
    * @tparam T data type
    */
  def writeCSV[T <: Any](data: Array[Array[T]],file:String,sep:String,header:Array[String] = Array.empty): Unit = {
    implicit val writerFormat: CSVFormat = new DefaultCSVFormat {
      override val delimiter: Char = sep.charAt(0)
      override val quoteChar: Char = '"'
    }

    val towrite = if(header.length == 0){data.map{_.map{_.toString}.toSeq}.toSeq} else {Seq(header.toSeq)++data.map{_.toSeq}.toSeq}

    CSVWriter.open(file)(writerFormat).writeAll(towrite)
  }

}
