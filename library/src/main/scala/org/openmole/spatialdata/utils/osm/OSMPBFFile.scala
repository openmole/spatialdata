package org.openmole.spatialdata.utils.osm

import java.io.{File, FileInputStream}

import crosby.binary.BinaryParser
import crosby.binary.Osmformat._
import crosby.binary.file.BlockInputStream
import org.locationtech.jts.geom.{Coordinate, GeometryFactory}
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.vector.{Lines, Points}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * See https://wiki.openstreetmap.org/wiki/PBF_Format for pbf format specification
  */
object OSMPBFFile {

  /**
    *
    * Reads nodes as Point and ways as LineString
    *
    * @param file file
    * @return
    */
  def readPBFFile(file: String): (Points,Lines) = {
    val parser = new PBFParser
    new BlockInputStream(new FileInputStream(new File(file)), parser).process()
    (parser.getNodes, parser.getWays)
  }

  private class PBFParser extends BinaryParser {
    private val ways: mutable.HashSet[(Seq[Long],Map[String,String])] = new mutable.HashSet
    private val nodes: mutable.HashMap[Long,(Double,Double,Map[String,String])] = new mutable.HashMap

    private val factory = new GeometryFactory

    def getWays: Lines = {
      val waysSeq = ways.toSeq
      Lines(
        waysSeq.map{case (ids,_) =>
          factory.createLineString(ids.map{l =>
            val n = nodes.getOrElse(l,(0.0,0.0,Map.empty))
            new Coordinate(n._1,n._2)
          }.toArray)
        },
        waysSeq.map(_._2)
      )
    }

    def getNodes: Points = {
      val nodesSeq = nodes.values.toSeq
      Points(
        nodesSeq.map{ case (x,y,_) => factory.createPoint(new Coordinate(x,y))},
        nodesSeq.map(_._3)
      )
    }

    protected override def parseRelations(rels: java.util.List[Relation]): Unit  = {
      //rels.asScala.map{r: Relation => r.getKeysList} // do nothing
    }

    /**
      * No attributes for dense nodes?
      *  -> parse keyval array with delimiter '0'
      * @param denseNodes dense nodes
      */
    protected override def parseDense(denseNodes: DenseNodes): Unit = {
      var (id,lat,lon): (Long,Long,Long) = (0L,0L,0L)
      (0 until denseNodes.getIdCount).foreach{i =>
        id += denseNodes.getId(i)
        lat += denseNodes.getLat(i)
        lon += denseNodes.getLon(i)
        nodes.put(id,(parseLon(lon), parseLat(lat),Map.empty))
      }
    }

    protected override def parseNodes(ndes: java.util.List[Node]): Unit = {
      ndes.asScala.foreach{
        n =>
          val attrs: Map[String,String] = (0 until n.getKeysCount).map(i => (getStringById(n.getKeys(i)),getStringById(n.getVals(i)))).toMap
          nodes.put(n.getId,(parseLon(n.getLon), parseLat(n.getLat), attrs))
      }
    }

    protected override def parseWays(wys: java.util.List[Way]): Unit = {
      wys.asScala.foreach { w =>
        val attrs: Map[String, String] = (0 until w.getKeysCount).map(i => (getStringById(w.getKeys(i)), getStringById(w.getVals(i)))).toMap
        val nodesids: Seq[Long] = utils.math.cumsum(w.getRefsList.asScala.toSeq.map(_.asInstanceOf[Long]))(Numeric.LongIsIntegral)
        ways.add((nodesids,attrs))
      }
    }

    protected override def parse(header: HeaderBlock): Unit = {
      utils.log("Reading osm pbf")
    }

    protected override def complete(): Unit = {
      utils.log("Finished parsing osm pbf - read "+nodes.size+" nodes and "+ways.size+" ways")
    }


  }



}
