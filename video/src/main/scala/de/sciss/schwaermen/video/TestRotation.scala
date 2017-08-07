/*
 *  TestRotation.scala
 *  (Schwaermen)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.schwaermen
package video

import java.awt.geom.{AffineTransform, Path2D, PathIterator}
import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import javax.swing.Timer

import scala.annotation.switch
import scala.swing.{BorderPanel, Button, Component, FlowPanel, Graphics2D, MainFrame, Swing, ToggleButton}
import Swing._
import scala.swing.event.ButtonClicked

object TestRotation {
  def loadText(): String = {
//    val url = getClass.getClassLoader.getResource("/de/sciss/schwaermen/text1.txt")
//    val is  = getClass.getClassLoader.getResourceAsStream("/de/sciss/schwaermen/text1.txt")
//    val sz  = is.available()
//    val arr = new Array[Byte](sz)
//    is.read(arr)
//    is.close()
//    new String(arr, "UTF-8")
    Util.readTextResource("text1.txt")
  }

  def run(): Unit = {
    val text    = loadText()
    val font    = Glyphosat.mkFont(64f)
    val gl      = Glyphosat(text, font)

    val comp = new Component {
      background = Color.black
      foreground = Color.white
      opaque     = true

      preferredSize = (800, 400)

      override protected def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        val w = peer.getWidth
        val h = peer.getHeight
        g.setColor(background)
        g.fillRect(0, 0, w, h)
        g.setColor(foreground)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )

//        val sx = (w - 8).toDouble / 800
//        val sy = (h - 8).toDouble / 400
//        val scale = math.min(sx, sy)
//        g.translate(4, 4)
//        g.scale(scale, scale)
        val atOrig = g.getTransform
        g.drawLine(200, 0, 200, h)
        g.drawLine(600, 0, 600, h)
        g.translate(200, 0)
        gl.render(g)
        g.setTransform(atOrig)
      }
    }

    def tick(): Unit = {
      gl.step()
      comp.repaint()
      // tk.sync()
    }

    val t = new Timer(30, Swing.ActionListener { _ => tick() })
//    t.start()

    val ggAnim = new ToggleButton("Anim") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          if (selected) t.restart() else t.stop()
      }
    }
    val ggTick = Button("Tick")(tick())

    val pBottom = new FlowPanel(ggAnim, ggTick)

    new MainFrame {
      contents = new BorderPanel {
        add(comp    , BorderPanel.Position.Center)
        add(pBottom , BorderPanel.Position.South )
      }
      pack().centerOnScreen()
      open()
    }
  }

  def runOLD(): Unit = {
    val text    = loadText()

    val tmpImg  = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val tmpG    = tmpImg.createGraphics()
    val font    = Glyphosat.mkFont(64f)
    val fm      = tmpG.getFontMetrics(font)
    val frc     = fm.getFontRenderContext
    val gv      = font.createGlyphVector(frc, text.take(32))
    val shape   = gv.getOutline
    val rectIn  = shape.getBounds

    val extentIn = math.max(rectIn.width, rectIn.height) * 0.33 // 0.2515

    val p = new PolarTransform(inWidth = extentIn /* rectIn.width */, inHeight = extentIn /* rectIn.height */,
      innerRadius = 0.0, angleStart = 0.0, angleSpan = math.Pi * 0.5,
      cx = extentIn * 0.5 /* rectIn.width */ * 0.5 /* rectIn.getCenterX */,
      cy = extentIn * 0.5 /* rectIn.height */ * 0.5 /* rectIn.getCenterY */, flipX = true, flipY = true)

    val arr   = new Array[Double](6)
    val path  = new Path2D.Double()
    var angle = 0.0

    def updatePath(): Unit = {
      path.reset()
      p.angleStart = angle
      val it    = shape.getPathIterator(AffineTransform.getTranslateInstance(-rectIn.x, -rectIn.y))

      while (!it.isDone) {
        val tpe = it.currentSegment(arr)
        (tpe: @switch) match {
          case PathIterator.SEG_MOVETO  =>
            p(arr, 0)
            path.moveTo(arr(0), arr(1))

          case PathIterator.SEG_LINETO  =>
            p(arr, 0)
            path.lineTo(arr(0), arr(1))

          case PathIterator.SEG_QUADTO  =>
            p(arr, 0)
            p(arr, 2)
            path.quadTo(arr(0), arr(1), arr(2), arr(3))

          case PathIterator.SEG_CUBICTO =>
            p(arr, 0)
            p(arr, 2)
            p(arr, 4)
            path.curveTo(arr(0), arr(1), arr(2), arr(3), arr(4), arr(5))

          case PathIterator.SEG_CLOSE   =>
            path.closePath()
        }
        it.next()
      }
    }

    updatePath()
    val rectOut = path.getBounds
    //    println(rectOut)

    val comp = new Component {
      background = Color.black
      foreground = Color.white
      opaque     = true

      preferredSize = {
        val d = rectOut.getSize
        d.width  += 8
        d.height += 8
        d.width   = math.min(1290, d.width)
        d.height  = math.min(1080, d.height)
        d
      }

      override protected def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        val w = peer.getWidth
        val h = peer.getHeight
        g.setColor(background)
        g.fillRect(0, 0, w, h)
        g.setColor(foreground)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )

        //          val sx = w.toDouble / rectOut.width
        //          val sy = h.toDouble / rectOut.height
        val sx = (w - 8).toDouble / rectOut.getMaxX
        val sy = (h - 8).toDouble / rectOut.getMaxY
        val scale = math.min(sx, sy)
        g.translate(4, 4)
        g.scale(scale, scale)
        //          g.translate(4 - rectOut.x , 4 - rectOut.y)
        g.fill(path)
      }
    }

    new MainFrame {
      contents = comp
      pack().centerOnScreen()
      open()
    }

    val Pi2     = math.Pi * 2
    val angStep = -0.2 * math.Pi / 180 + Pi2
    //    val tk      = comp.peer.getToolkit

    val t = new Timer(30, Swing.ActionListener { _ =>
      angle = (angle + angStep) % Pi2
      updatePath()
      comp.repaint()
      // tk.sync()
    })
    t.start()
  }
}