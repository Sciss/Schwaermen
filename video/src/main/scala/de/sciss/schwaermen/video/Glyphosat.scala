/*
 *  Glyphosat.scala
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

import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.{Font, Shape}

import de.sciss.kollflitz
import de.sciss.kollflitz.Vec
import de.sciss.schwaermen.video.Glyphosat.{CharInfo, CharVertex}

import scala.collection.{Map, Set, breakOut}
import scala.swing.Graphics2D

object Glyphosat {
  private[this] lazy val _initFont: Font = {
    val url = getClass.getResource("/OpenSans-CondLight.ttf")
    require(url != null)
    val is = url.openStream()
    val res = Font.createFont(Font.TRUETYPE_FONT, is)
    is.close()
    res
  }

  private[this] var _condensedFont: Font = _

  def mkFont(size: Float): Font = {
    if (_condensedFont == null) _condensedFont = _initFont
    _condensedFont.deriveFont(size)
  }

  def apply(text: String, font: Font): Glyphosat = {
    val tmpImg  = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val tmpG    = tmpImg.createGraphics()
    val fm      = tmpG.getFontMetrics(font)
    val frc     = fm.getFontRenderContext

    import kollflitz.Ops._

    val charSet   : Set[Char]           = text.toSet
    val charShapes: Map[Char, CharInfo] = charSet.map { c =>
      val gv      = font.createGlyphVector(frc, c.toString)
      val shape   = gv.getOutline
      val bounds  = gv.getLogicalBounds
      c -> CharInfo(c, shape, bounds)
    } (breakOut)

    val charPairs: Set[(Char, Char)]  = (text: Vec[Char]).mapPairs((a, b) => (a, b))(breakOut): Set[(Char, Char)]

    val charPairSpacing: Map[(Char, Char), Double] = charPairs.map { case pair @ (a, b) =>
      val gv    = font.createGlyphVector(frc, s"$a$b")
      val pA    = gv.getGlyphPosition(0)
      val pB    = gv.getGlyphPosition(1)
      val dist  = pB.getX - pA.getX
      pair -> dist
    } (breakOut)

    tmpG.dispose()

    val textA = testDropWritten(text)

    val characters: Array[CharInfo]   = charShapes.values.toArray.sortBy(_.c)
    val charIdxMap: Map[Char, Int]    = characters.iterator.map(_.c).zipWithIndex.toMap
    val words: Array[Array[Int]]      = textA.split(" ").map { s =>
      s.map(charIdxMap)(breakOut): Array[Int]
    }

    new Glyphosat(
      charShapes      = charShapes,
      charPairSpacing = charPairSpacing,
      words           = words,
      characters      = characters
    )
  }

  // resolves the parentheses and drops the right hand side of (lhs|rhs) bits
  private def testDropWritten(s: String): String = {
    var j   = 0
    var res = s
    while ({ j = res.indexOf('('); j >= 0 }) {
      val k = res.indexOf('|')
      val m = res.indexOf(')', k + 1) + 1
      res = res.substring(0, j) + res.substring(j + 1, k) + res.substring(m)
    }
    while ({ j = res.indexOf("  "); j >= 0 }) {
      res = res.substring(0, j) + res.substring(j + 1)
    }
    res
  }

  private final case class CharInfo(c: Char, shape: Shape, bounds: Rectangle2D) {
    val left  : Float = bounds.getMinX.toFloat
    val right : Float = bounds.getMaxX.toFloat
    val top   : Float = bounds.getMinX.toFloat
    val bottom: Float = bounds.getMaxY.toFloat
  }

//  private final class Word(val index: Int) {
//    var characters: Array[CharVertex] = _
//  }

  /*

    spring between two masses:
    https://physics.stackexchange.com/questions/61809/two-masses-attached-to-a-spring#61813

   */

  private final class CharVertex(val info: CharInfo, val wordIndex: Int) {
    var  x: Float = 0f  // position x
    var  y: Float = 0f  // position y
    var vx: Float = 0f  // velocity x
    var vy: Float = 0f  // velocity y
//    var ax: Float = 0f  // acceleration x
//    var ay: Float = 0f  // acceleration y

    var succ: CharVertex = _
  }
}
final class Glyphosat private(charShapes      : Map[Char        , CharInfo],
                              charPairSpacing : Map[(Char, Char), Double  ],
                              words: Array[Array[Int]], characters: Array[CharInfo]) {
  private[this] var head: CharVertex = _

  private[this] val spaceChar = charShapes(' ')

  private[this] val NominalY    = 64f // 16f
  private[this] val NominalYK   = 0.1f
  private[this] val NominalVX   = -1f
  private[this] val NominalVXK  = 0.1f
  private[this] val PairLXK     = 0.15f
  private[this] val PairRXK     = 0.05f
//  private[this] val PairXL      = 0.0f // spaceChar.bounds.getWidth.toFloat
  private[this] val PairLYK     = 0.15f
  private[this] val PairRYK     = 0.05f
  private[this] val DragM       = 1.0f - 0.1f
//  private[this] val PairYL      = 0.0f
  private[this] val ScreenWidth = 400f

  private def popWord(wordIndex: Int, pred0: CharVertex): Unit = {
    val word  = words(wordIndex)
    val ch0   = characters(word(0))
    val v0    = new CharVertex(ch0, wordIndex = wordIndex) // XXX TODO --- could avoid allocation
    var pred  = pred0
    if (pred == null) {
      head      = v0
      v0.x       = ScreenWidth
      v0.y       = NominalY
    } else {
      pred.succ = v0
      v0.x       = math.max(ScreenWidth, pred.x + pred.info.right)
      v0.y       = pred.y
    }
    var wi = 1
    pred = v0
    while (wi <= word.length) {
      val ch    = if (wi < word.length) characters(word(wi)) else spaceChar
      val v     = new CharVertex(ch, wordIndex = wordIndex)
      pred.succ = v
      v.x       = pred.x + pred.info.right
      v.y       = pred.y
      pred      = v
      wi       += 1
    }
  }

  def render(g: Graphics2D): Unit = {
    var curr = head
    var px = 0f
    var py = 0f
    while (curr != null) {
      g.translate(curr.x - px, curr.y - py)
      g.fill(curr.info.shape)
      px = curr.x
      py = curr.y
      curr = curr.succ
      if (curr == head) sys.error("CYCLIC")
    }
  }

  def step(): Unit = {
    var curr = head
    if (curr == null) {
      popWord(0, null)
      curr = head
    }

    // ---- updates velocities ----

    curr.vx += (NominalVX - curr.vx) * NominalVXK
    curr.vy += (NominalY  - curr. y) * NominalYK
    curr.vx *= DragM
    curr.vy *= DragM
    var pred = curr
    curr = curr.succ

    var FOO = false

    while (curr != null) {
      val x1 = pred.x + pred.info.right
      val x2 = curr.x + pred.info.left
      val y1 = pred.y
      val y2 = curr.y
      val dx = x1 - x2
      val dy = y1 - y2
      val dvx = dx * PairLXK
      val dvy = dy * PairLYK
      curr.vx *= DragM
      curr.vy *= DragM
      curr.vx += dvx
      curr.vy += dvy
      if (FOO) {
        pred.vx -= dx * PairRXK
        pred.vy -= dy * PairRYK
      } else {
        FOO = true
      }
      pred = curr
      curr = curr.succ
    }

    // ---- update positions ----

    curr = head
    pred = curr
    var allOut  = true
    var predIdx = -1
    while (curr != null) {
      curr.x += curr.vx
      curr.y += curr.vy
      if (allOut) {
        if (curr.wordIndex != predIdx) {
          head    = curr // drop previous head
          predIdx = curr.wordIndex
        }
        val currOut = curr.x + curr.info.right <= 0 || curr.y + curr.info.bottom <= 0
        allOut &= currOut
      }
      pred    = curr
      curr    = curr.succ
    }

    if (pred.x + pred.info.right < ScreenWidth) {
      popWord((pred.wordIndex + 1) % words.length, pred)
    }
  }
}