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

import java.awt.RenderingHints
import java.awt.image.BufferedImage

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
    val gv      = font.createGlyphVector(frc, text.take(27))
    val shape   = gv.getOutline
    val rect    = shape.getBounds

    new MainFrame {
      contents = new Component {
        preferredSize = {
          val d = rect.getSize
          d.width  += 8
          d.height += 8
          d
        }

        override protected def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
          g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )
          g.translate(4 - rect.x , 4 - rect.y)
          g.fill(shape)
        }
      }
      pack().centerOnScreen()
      open()
    }
  }
}