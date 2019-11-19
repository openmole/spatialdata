package org.openmole.spatialdata.utils

import org.openmole.spatialdata.network.Network
import javax.swing._
import java.awt._
import java.lang
import java.lang.System

import javax.swing.plaf.ComponentUI

package object visualization {

  case class NetworkFrame(network: Network, frameWidth: Int = 500, frameHeight: Int = 500, margin: Int = 50) extends JFrame() {
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
              //println(margin + (n.x * frameWidth).toInt,margin + (n.y*frameHeight).toInt)
              gg.fillRect(margin + (n.x * frameWidth).toInt,margin + (n.y*frameHeight).toInt,5,5)
            }}
            network.links.foreach{l => gg.drawLine(margin + (l.e1.x*frameWidth).toInt,margin + (l.e1.y*frameHeight).toInt,margin + (l.e2.x*frameWidth).toInt,margin + (l.e2.y*frameHeight).toInt)}
          }
        })
        setOpaque(true)
        setForeground(Color.BLACK)
        setBackground(Color.WHITE)
      })
      setVisible(true)
      println("visible")
      Thread.sleep(10000)
    }
  }


  /**
    * Quick visu for debugging purposes
    * @param network
    */
  def staticVisualization(network: Network): Unit = {
    val frame = NetworkFrame(network)
    println("frame created")
    frame.init
  }


}
