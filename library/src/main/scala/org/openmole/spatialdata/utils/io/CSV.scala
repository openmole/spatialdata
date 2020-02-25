
package org.openmole.spatialdata.utils.io

import java.io.{BufferedReader, File, FileReader}

import breeze.linalg.CSCMatrix
import com.github.tototoshi.csv._
import org.apache.commons.math3.linear
import org.openmole.spatialdata.utils.math.SparseMatrix.{SparseBreeze, SparseCommons}
import org.openmole.spatialdata.utils.math.{BreezeSparseMatrix, SparseMatrix, SparseMatrixImpl}

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
    * filter a dense mat as sparse from csv
    *  for perf reasons, use directly builder for sparse breeze here => specific implementation!
    *  *spec: first line contains n,p*
    * @param file
    * @param filter
    * @param sep
    * @param naformat
    * @return
    */
  def readSparseMatFromDense(file: String,
                             filter: Double => Boolean,
                             sep: String=",",
                             naformat: String = "NA"
                            )(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SparseMatrix = {
    val r = new BufferedReader(new FileReader(new File(file)))
    val res = new ArrayBuffer[Array[Double]]
    var currentline = r.readLine()
    val rawdims = currentline.split(sep)
    val (n,p) = (rawdims(0).toInt,rawdims(1).toInt)

    spMatImpl match {
      case _: SparseBreeze => {
        val breezebuilder = new CSCMatrix.Builder[Double](rows = n, cols = p)
        currentline = r.readLine()
        var i = 0
        while (currentline != null) {
          currentline.split(sep).zipWithIndex.map { case (s, j) =>
            if (s.equals("NA")) None else {
              if (filter(s.toDouble)) Some((i, j, s.toDouble)) else None
            }
          }.filter(_.isDefined).foreach { case Some((i, j, v)) => breezebuilder.add(i, j, v) }
          currentline = r.readLine()
          i = i + 1
        }
        BreezeSparseMatrix(breezebuilder.result())
      }
      case _: SparseCommons => {
        val m:linear.OpenMapRealMatrix = new linear.OpenMapRealMatrix(n,p)
        currentline = r.readLine()
        var i = 0
        while (currentline != null) {
          currentline.split(sep).zipWithIndex.map { case (s, j) =>
            if (s.equals("NA")) None else {
              if (filter(s.toDouble)) Some((i, j, s.toDouble)) else None
            }
          }.filter(_.isDefined).foreach { case Some((i, j, v)) => m.setEntry(i,j,v) }
          currentline = r.readLine()
          i = i + 1
        }
        SparseMatrixImpl(m)
      }
    }


  }

  /**
    * Read a sparse mat as a csv (i,j,v)
    *  **Spec: first line is (N,P)**
    * @param file
    * @param sep
    * @param naformat
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
    //val n = aentries.map(_._1).max // this is not correct if last columns/rows are empty
    //val p = aentries.map(_._2).max
    SparseMatrix(aentries,n,p)
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
