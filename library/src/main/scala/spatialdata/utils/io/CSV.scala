
package spatialdata.utils.io

import com.github.tototoshi.csv._


object CSV {

  /**
    * read csv file
    * @param file
    * @param sep
    * @return
    */
  def readCSV(file: String,sep: String): Map[String,Seq[String]] = {

    implicit val readerFormat = new DefaultCSVFormat {
      override val delimiter = sep.charAt(0)
      override val quoteChar: Char = '"'
    }
    val (header,content) = CSVReader.open(file)(readerFormat).allWithOrderedHeaders()
    header.map{case s => (s,content.map{_.get(s).get})}.toMap
  }

  def writeCSV(data: Array[Array[Double]],file:String,sep:String) = {
    implicit val writerFormat = new DefaultCSVFormat {
      override val delimiter = sep.charAt(0)
      override val quoteChar: Char = '"'
    }
    CSVWriter.open(file)(writerFormat).writeAll(data.map{_.toSeq}.toSeq)
  }

}
