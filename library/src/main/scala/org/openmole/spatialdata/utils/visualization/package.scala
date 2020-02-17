package org.openmole.spatialdata.utils

import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.vector.Point
import org.openmole.spatialdata.network.{Network, Node}
import javax.swing._
import java.awt._
import java.lang
import java.lang.System

import javax.swing.plaf.ComponentUI
import org.openmole.spatialdata.model.spatialinteraction.SpatialInteractionModel

package object visualization {

  implicit val doubleOrdering = Ordering.Double.TotalOrdering

  import Color._
  val colors = Vector(BLACK,BLUE,MAGENTA,ORANGE,PINK,RED,CYAN,YELLOW,DARK_GRAY,GRAY,GREEN,LIGHT_GRAY)//,WHITE)

  def palette(i: Int): Color = {colors(i%colors.size)}


  /**
    * Quick visu for debugging purposes
    * @param network
    */
  def staticNetworkVisualization(networks: Seq[Network],
                                 withLabel: Boolean = false,
                                 edgeColors: Seq[Int] = Seq.empty,
                                 nodeColorClasses: Node=>Int={_ => 0},
                                 nodePositioning: Node => Point = {n => n.position}
                                ): Unit = {
    val frame = NetworkFrame(networks = networks,withLabel=withLabel,edgeColors=edgeColors,nodeColorClasses=nodeColorClasses,nodePositioning=nodePositioning)
    frame.init
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
