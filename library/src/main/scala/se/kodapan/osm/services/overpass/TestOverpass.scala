package se.kodapan.osm.services.overpass

import spatialdata.osm.APIExtractor.Buildings.asPolygonSeq

object TestOverpass extends App {

  import se.kodapan.osm.domain.Way
  import se.kodapan.osm.domain.root.PojoRoot
  import se.kodapan.osm.parser.xml.instantiated.InstantiatedOsmXmlParser
  import se.kodapan.osm.services.overpass.Overpass
  import java.io.StringReader

  val overpass = new Overpass
  overpass.setUserAgent("test suite of <https://github.com/karlwettin/osm-common/>");
  overpass.open()

  val root = new PojoRoot
  val parser = InstantiatedOsmXmlParser.newInstance
  parser.setRoot(root)

  parser.parse(new StringReader(overpass.execute(
    """
      |<union>
      |  <bbox-query s="51.249" w="7.148" n="51.251" e="7.152"/>
      |  <recurse type="up"/>
      |</union>
      |<print mode="meta"/>
    """.stripMargin)))
  asPolygonSeq(root.enumerateWays).foreach(println)
}
