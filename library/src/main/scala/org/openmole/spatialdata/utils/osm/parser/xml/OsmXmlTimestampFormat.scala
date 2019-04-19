package org.openmole.spatialdata.utils.osm.parser.xml

import java.text.{DateFormat, FieldPosition, ParsePosition, SimpleDateFormat}
import java.util.Date


/**
  * @author kalle
  * @since 2013-05-01 15:59
  */
object OsmXmlTimestampFormat {
  private val format1 = "yyyy-MM-dd'T'HH:mm:ss'Z'"
  private val format2 = "yyyy-MM-dd'T'HH:mm:ss"
}

class OsmXmlTimestampFormat() extends DateFormat {
  private val implementation1 = new SimpleDateFormat(OsmXmlTimestampFormat.format1)
  private val implementation2 = new SimpleDateFormat(OsmXmlTimestampFormat.format2)

  override def format(date: Date, stringBuffer: StringBuffer, fieldPosition: FieldPosition) = implementation1.format(date, stringBuffer, fieldPosition)

  override def parse(s: String, parsePosition: ParsePosition): Date = {
    if (s.length - parsePosition.getIndex == OsmXmlTimestampFormat.format1.length)
      return implementation1.parse(s, parsePosition)
    implementation2.parse(s, parsePosition)
  }
}