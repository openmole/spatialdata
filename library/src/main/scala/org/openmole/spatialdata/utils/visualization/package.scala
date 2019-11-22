package org.openmole.spatialdata.utils

import org.openmole.spatialdata.network.{Network, Node}
import javax.swing._
import java.awt._
import java.lang
import java.lang.System

import javax.swing.plaf.ComponentUI

package object visualization {

  import Color._
  val colors = Vector(BLACK,BLUE,MAGENTA,ORANGE,PINK,RED,CYAN,YELLOW,DARK_GRAY,GRAY,GREEN,LIGHT_GRAY)//,WHITE)

  def palette(i: Int): Color = {colors(i%colors.size)}

  case class NetworkFrame(network: Network,
                          frameWidth: Int = 600,
                          frameHeight: Int = 600,
                          nodeSize: Int = 6,
                          margin: Int = 50,
                          withLabel: Boolean = true,
                          nodeColorClasses: Option[Node => Int] = None
                         ) extends JFrame() {
    def init: Unit = {
      frameInit()
      setSize(frameWidth+2*margin,frameHeight+2*margin)
      setLocation(100,100)
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      add(new JComponent {
        setUI(new ComponentUI {
          override def paint(g: Graphics, c: JComponent): Unit = {
            val gg: Graphics2D = g.create.asInstanceOf[Graphics2D]
            gg.setColor(Color.BLACK)
            gg.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
            gg.setStroke(new BasicStroke(1, BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND))
            network.nodes.foreach{n => {
              val (x,y) = (margin + (n.x * frameWidth - nodeSize/2).toInt,margin + (n.y*frameHeight-nodeSize/2).toInt)
              if (nodeColorClasses.isDefined) gg.setColor(palette(nodeColorClasses.get(n)))
              gg.fillRect(x,y,nodeSize,nodeSize)
              if (withLabel) gg.drawString(n.id.toString,x+nodeSize,y+nodeSize)
            }}
            gg.setColor(Color.BLACK) // link no color by default
            network.links.foreach{l => gg.drawLine(margin + (l.e1.x*frameWidth).toInt,margin + (l.e1.y*frameHeight).toInt,margin + (l.e2.x*frameWidth).toInt,margin + (l.e2.y*frameHeight).toInt)}
          }
        })
        setOpaque(true)
        setForeground(Color.BLACK)
        setBackground(Color.WHITE)
      })
      setVisible(true)
      Thread.sleep(10000)
    }
  }


  /**
    * Quick visu for debugging purposes
    * @param network
    */
  def staticVisualization(network: Network,withLabel: Boolean = false,nodeColorClasses: Option[Node=>Int]=None): Unit = {
    val frame = NetworkFrame(network = network,withLabel=withLabel,nodeColorClasses=nodeColorClasses)
    frame.init
  }


}
