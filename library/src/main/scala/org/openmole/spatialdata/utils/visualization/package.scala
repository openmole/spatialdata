package org.openmole.spatialdata.utils

import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.vector.{Point, Polygons}
import org.openmole.spatialdata.network.{Link, Network, Node}
import java.awt._

import org.openmole.spatialdata.model.spatialinteraction.SpatialInteractionModel

package object visualization {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  import Color._
  val colors = Vector(BLACK,BLUE,MAGENTA,ORANGE,PINK,RED,CYAN,YELLOW,DARK_GRAY,GRAY,GREEN,LIGHT_GRAY)//,WHITE)

  def palette(i: Int): Color = {colors(i%colors.size)}


  def staticVectorVisualization(
                                 networks: Seq[Network] = Seq.empty,
                                 withLabel: Boolean = false,
                                 edgeColoring: Link => Int = {_ => 0},
                                 edgeScaling: Link => Double = {_ => 2.0},
                                 nodeColoring: Node=>Int={_ => 0},
                                 nodePositioning: Node => Point = {n => n.position},
                                 nodeScaling: Node => Double = {_ => 1.0},
                                 nodeShaping: Node => Int = {_ => 0},
                                 polygons: Seq[Polygons] = Seq.empty,
                                 polygonsScaleColoringAttributes: Seq[String] = Seq.empty
                               ): Unit = {
    if (org.openmole.spatialdata.HEADLESS) return
    val frame = VectorFrame(networks = networks,withLabel=withLabel,edgeColoring=edgeColoring,edgeScaling=edgeScaling,nodeColoring=nodeColoring,nodePositioning=nodePositioning, nodeScaling=nodeScaling, nodeShaping=nodeShaping,
      polygons=polygons, polygonsScaleColoringAttributes=polygonsScaleColoringAttributes
    )
    frame.init()
  }

  /**
    * Network visualization
    * @param networks set of networks
    * @param withLabel display labels
    * @param edgeColoring classes for edge colo
    * @param edgeScaling edge size
    * @param nodeColoring classes for node color
    * @param nodePositioning node position
    * @param nodeScaling node rescale
    * @param nodeShaping node shape (0:square; 1:circle)
    */
  def staticNetworkVisualization(networks: Seq[Network],
                                 withLabel: Boolean = false,
                                 edgeColoring: Link => Int = {_ => 0},
                                 edgeScaling: Link => Double = {_ => 2.0},
                                 nodeColoring: Node=>Int={_ => 0},
                                 nodePositioning: Node => Point = {n => n.position},
                                 nodeScaling: Node => Double = {_ => 1.0},
                                 nodeShaping: Node => Int = {_ => 0}
                                ): Unit = {
    if (org.openmole.spatialdata.HEADLESS) return
    val frame = NetworkFrame(networks = networks,withLabel=withLabel,edgeColoring=edgeColoring,edgeScaling=edgeScaling,nodeColoring=nodeColoring,nodePositioning=nodePositioning, nodeScaling=nodeScaling, nodeShaping=nodeShaping)
    frame.init()
  }

  def normalizedPositionNode(networks: Seq[Network] = Seq.empty, polygons: Seq[Polygons] = Seq.empty): Node => Point = {
    //val (xcoordsnw,ycoordsnw) = networks.flatMap(_.nodes.toSeq.map(_.position)).unzip
    //val (xcoordspolys,ycoordspolys) = polygons.flatMap(_.polygons.flatMap(_.getCoordinates.toSeq.map(c => (c.x,c.y)))).unzip
    //val (xcoords,ycoords) = (xcoordsnw++xcoordspolys, ycoordsnw++ycoordspolys)
    // note: not efficient at all to recompute for each node
    val (xcoords,ycoords) = networks.flatMap(_.nodes.toSeq.map(_.position)).unzip
    val (minx,maxx,miny,maxy) = (xcoords.min,xcoords.max,ycoords.min,ycoords.max)
    n: Node => ((n.position._1 - minx) / (maxx - minx),(n.position._2 - miny) / (maxy - miny))
  }

  //def normalizedPositionPolygon(networks: Seq[Network] = Seq.empty, polygons: Seq[Polygons] = Seq.empty): Polygon => Polygon

  def staticFlowsVisualization(model: SpatialInteractionModel
                                ): Unit = {
    if (org.openmole.spatialdata.HEADLESS) return
    val frame = FlowsFrame(model)
    frame.init()
  }


  def normalization(r: Array[Array[Double]]): Array[Array[Double]] = {
    val (mi,ma) = (r.flatten.min,r.flatten.max)
    r.map{_.map{d: Double => (d - mi) / (ma - mi)}}
  }

  def normalizationLog: Array[Array[Double]] => Array[Array[Double]] = {r: Array[Array[Double]] =>visualization.normalization(r.map{_.map{d => if(d<=0.0) 0.0 else scala.math.log10(d)}})}

  def staticRasterVisualization(raster: RasterLayerData[Double]//,
                                //projection: RasterLayerData[Double] => RasterLayerData[Double] = normalization
                               ): Unit = {
    if (org.openmole.spatialdata.HEADLESS) return
    val frame = RasterFrame(raster)
    frame.init()
  }



}
