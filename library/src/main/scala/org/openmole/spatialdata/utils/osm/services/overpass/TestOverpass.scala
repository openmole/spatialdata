package org.openmole.spatialdata.utils.osm.services.overpass

import org.openmole.spatialdata.utils.osm.APIExtractor.Buildings.asPolygonSeq
import org.openmole.spatialdata.utils.osm.domain.root.PojoRoot
import org.openmole.spatialdata.utils.osm.parser.xml.instantiated.InstantiatedOsmXmlParser

object TestOverpass extends App {

  import java.io.StringReader

  import org.openmole.spatialdata.utils.osm.domain.Way

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
