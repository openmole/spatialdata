package se.kodapan.osm.parser.xml


/**
  * @author kalle
  * @since 2013-05-01 21:39
  */
class OsmXmlParserException(s: String, throwable: Throwable) extends Exception(s, throwable) {
  def this(s: String) {
    this(s, null)
  }
  def this(throwable: Throwable) {
    this("", throwable)
  }
}
