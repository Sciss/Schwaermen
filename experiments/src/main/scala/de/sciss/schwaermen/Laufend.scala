/*
 *  Laufend.scala
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

import java.awt.{Color, RenderingHints}
import java.awt.geom.{AffineTransform, Path2D, PathIterator}
import java.awt.image.BufferedImage
import javax.swing.Timer

import scala.annotation.switch
import scala.swing.{Component, Graphics2D, MainFrame, Swing}

object Laufend {
  final case class Config()

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Schwaermen-Laufend") {
//      opt[File]("base-dir")
//        .text (s"Base directory (default: ${default.baseDir})")
//        .action { (f, c) => c.copy(baseDir = f) }

//      opt[Unit] ('d', "dump-osc")
//        .text (s"Enable OSC dump (default ${default.dumpOSC})")
//        .action   { (_, c) => c.copy(dumpOSC = true) }

//      opt[Int] ("key-shutdown")
//        .text (s"Keypad key to trigger shutdown (1 to 9; default ${default.keyShutdown})")
//        .validate(i => if (i >= 1 && i <= 9) Right(()) else Left("Must be 1 to 9") )
//        .action { (v, c) => c.copy(keyShutdown = (v + '0').toChar) }

    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      Swing.onEDT(run(config))
    }
  }

  private[this] val text =
    """es geht und geht das gehen geht und vergeht sich im schritt aus dem schritt der abdruck verschwimmt wird weich am ende"""

  def run(config: Config): Unit = {
    val tmpImg  = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val tmpG    = tmpImg.createGraphics()
    val font    = MyFont(64)
    val fm      = tmpG.getFontMetrics(font)
    val frc     = fm.getFontRenderContext
    val gv      = font.createGlyphVector(frc, text.take(32))
    val shape   = gv.getOutline
    val rectIn  = shape.getBounds

    val extentIn = math.max(rectIn.width, rectIn.height) * 0.33 // 0.2515

    val p = new MyPolar(inWidth = extentIn /* rectIn.width */, inHeight = extentIn /* rectIn.height */,
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
    val tk      = comp.peer.getToolkit

    val t = new Timer(30, Swing.ActionListener { _ =>
      angle = (angle + angStep) % Pi2
      updatePath()
      comp.repaint()
      tk.sync()
    })
    t.start()
  }
}