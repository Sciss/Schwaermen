/*
 *  TransparentTest.scala
 *  (Schwaermen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.schwaermen

import java.awt.{BasicStroke, Color, Graphics, Graphics2D}

import scala.swing.Swing

object TransparentTest {
  def main(args: Array[String]): Unit = {
    Swing.onEDT(run())
  }

  def run(): Unit = {
    val frame = new javax.swing.JFrame("Test")
    frame.setUndecorated(true)
    frame.setBackground (new Color(0,0,0,0))
    frame.setContentPane(new TestComponent())
    frame.pack()
    frame.setSize(500, 500)
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
  }

  final class TestComponent extends javax.swing.JComponent {
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      g.setColor(Color.red)
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setStroke(new BasicStroke(3f))
      g.drawLine(0, 0, getWidth, getHeight)
      g.drawLine(0, getHeight, getWidth, 0)
    }
  }
}
