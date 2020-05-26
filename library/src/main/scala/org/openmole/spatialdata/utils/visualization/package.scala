package org.openmole.spatialdata.utils

import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.vector.Point
import org.openmole.spatialdata.network.{Link, Network, Node}
import java.awt._

import org.openmole.spatialdata.model.spatialinteraction.SpatialInteractionModel

package object visualization {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  import Color._
  val colors = Vector(BLACK,BLUE,MAGENTA,ORANGE,PINK,RED,CYAN,YELLOW,DARK_GRAY,GRAY,GREEN,LIGHT_GRAY)//,WHITE)

  def palette(i: Int): Color = {colors(i%colors.size)}


  /**
    * Quick visu for debugging purposes
    */
  def staticNetworkVisualization(networks: Seq[Network],
                                 withLabel: Boolean = false,
                                 edgeColorClasses: Link => Int = {_ => 0},
                                 nodeColorClasses: Node=>Int={_ => 0},
                                 nodePositioning: Node => Point = {n => n.position}
                                ): Unit = {
    val frame = NetworkFrame(networks = networks,withLabel=withLabel,edgeColorClasses=edgeColorClasses,nodeColorClasses=nodeColorClasses,nodePositioning=nodePositioning)
    frame.init
  }

  def normalizedPosition(networks: Seq[Network]): Node => Point = {
    val (xcoords,ycoords) = networks.flatMap(_.nodes.toSeq.map(_.position)).unzip
    val (minx,maxx,miny,maxy) = (xcoords.min,xcoords.max,ycoords.min,ycoords.max)
    n: Node => ((n.position._1 - minx) / (maxx - minx),(n.position._2 - miny) / (maxy - miny))
  }

  def staticFlowsVisualization(model: SpatialInteractionModel
                                ): Unit = {
    val frame = FlowsFrame(model)
    frame.init
  }


  def normalization(r: RasterLayerData[Double]): RasterLayerData[Double] = {
    val (mi,ma) = (r.flatten.min,r.flatten.max);r.map{_.map{d: Double => (d - mi) / (ma - mi)}}
  }


  def staticRasterVisualization(raster: RasterLayerData[Double],
                                projection: RasterLayerData[Double] => RasterLayerData[Double] = //normalization
                                 {r =>visualization.normalization(r.map{_.map{d => if(d<=0.0) 0.0 else scala.math.log10(d)}})}
                               ): Unit = {
    val frame = RasterFrame(raster)
    frame.init
  }



}
