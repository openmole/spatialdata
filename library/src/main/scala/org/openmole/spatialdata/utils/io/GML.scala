package org.openmole.spatialdata.utils.io

import org.openmole.spatialdata.network._

import java.io.{BufferedWriter, FileWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Graph modeling language: simple text format for networks
 *
 *  ! attributes x,y should be in graphics node attribute?
 *
 *    check https://github.com/alexaverbuch/graph_gen_utils
 */
object GML {

  /**
   * ! dirty implementation - 1 depth only
   *
   * @param file file
   * @return
   */
  def readGML(file: String, nodeAttributes: Seq[String]): Network = {
    val nodes = new mutable.HashMap[Int, Node]
    val links = new ArrayBuffer[Link]
    val s = Source.fromFile(file)
    var currentObject = ""
    var currentX = 0.0; var currentY = 0.0; var currentId = 0; var currentAttr = new mutable.HashMap[String, AnyRef]()
    var currentSource = 0; var currentTarget = 0; var currentLength = 0.0; var currentWeight = 0.0
    var depth = 0
    s.getLines().foreach{
      raw =>
        if(raw.contains("node")) currentObject = "node"
        if(raw.contains("edge")) currentObject = "edge"

        // node attributes
        if(raw.contains("x")) currentX = raw.trim.split(" ").last.toDouble
        if(raw.contains("y")) currentY = raw.trim.split(" ").last.toDouble
        if(raw.contains("id")) currentId = raw.trim.split(" ").last.toInt
        nodeAttributes.foreach(a => if (raw.contains(a)) currentAttr.put(a, raw.trim.split(" ").last))

        // edge attributes
        if(raw.contains("source")) currentSource = raw.trim.split(" ").last.toInt
        if(raw.contains("target")) currentTarget = raw.trim.split(" ").last.toInt
        if(raw.contains("weight")) currentWeight = raw.trim.split(" ").last.toDouble
        if(raw.contains("length")) currentLength = raw.trim.split(" ").last.toDouble
        if(raw.contains("[")) depth = depth + 1
        if(raw.contains("]")) {
          depth = depth - 1
          if (depth == 1) {
            currentObject match {
              case "node" => {
                nodes.put(currentId, Node(currentId, currentX, currentY, currentAttr.toMap))
                currentAttr = new mutable.HashMap[String, AnyRef]()
              }
              case "edge" => links.addOne(Link(nodes.getOrElse(currentSource, Node.empty), nodes.getOrElse(currentTarget, Node.empty), weight = currentWeight, length=currentLength, directed = false))
            }
          }
        }
    }
    s.close
    Network(nodes.values.toSet, links.toSet)
  }

  /**
   * write gml
   * @param network network
   * @param file file
   */
  def writeGML(network: Network,file: String): Unit = {
    val w = new BufferedWriter(new FileWriter(file, false))
    w.write("graph\n[")

    network.nodes.foreach{ n=>
      w.write(s"  node\n  [\n    id ${n.id}\n    x ${n.x}\n    y ${n.y}\n"+n.attributes.toSeq.map{case (k,v) => s"    $k $v"}.mkString("\n")+"\n  ]\n")
    }
    network.links.foreach{ l=>
      w.write(s"  edge\n  [\n    source ${l.e1.id}\n    target ${l.e2.id}\n    length ${l.length}\n    weight ${l.weight}\n  ]\n")
    }

    w.write("]")
    w.close()
  }


}
