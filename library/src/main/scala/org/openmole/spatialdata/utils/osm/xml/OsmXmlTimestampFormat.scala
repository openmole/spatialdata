package org.openmole.spatialdata.utils.osm.xml

import java.text.{DateFormat, FieldPosition, ParsePosition, SimpleDateFormat}
import java.util.Date


/**
  * formats for xml timestamp
  */
class OsmXmlTimestampFormat() extends DateFormat {

  private val format1 = "yyyy-MM-dd'T'HH:mm:ss'Z'"
  private val format2 = "yyyy-MM-dd'T'HH:mm:ss"

  private val implementation1 = new SimpleDateFormat(format1)
  private val implementation2 = new SimpleDateFormat(format2)

  override def format(date: Date, stringBuffer: StringBuffer, fieldPosition: FieldPosition) = implementation1.format(date, stringBuffer, fieldPosition)

  override def parse(s: String, parsePosition: ParsePosition): Date = {
    if (s.length - parsePosition.getIndex == format1.length)
      return implementation1.parse(s, parsePosition)
    implementation2.parse(s, parsePosition)
  }
}
