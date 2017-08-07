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
import de.sciss.schwaermen.video.Glyphosat.{Word, CharInfo, CharVertex}

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

    ??? // new Glyphosat(charShapes = charShapes, charPairSpacing = charPairSpacing)
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

  private final case class CharInfo(c: Char, shape: Shape, bounds: Rectangle2D)

  private final class Word(val index: Int) {
    var characters: Array[CharVertex] = _
  }

  /*

    spring between two masses:
    https://physics.stackexchange.com/questions/61809/two-masses-attached-to-a-spring#61813

   */

  private final class CharVertex(val info: CharInfo, val wordIndex: Int) {
    var  x: Float = 0f  // position x
    var  y: Float = 0f  // position y
    var vx: Float = 0f  // velocity x
    var vy: Float = 0f  // velocity y
    var ax: Float = 0f  // acceleration x
    var ay: Float = 0f  // acceleration y

    var succ: CharVertex = _
  }
}
final class Glyphosat private(charShapes      : Map[Char        , CharInfo],
                              charPairSpacing : Map[(Char, Char), Double  ],
                              wordsFirstChar: Array[Int], characters: Array[CharVertex]) {
  private[this] var head: CharVertex = _

  private def popWord(i: Int, pred0: CharVertex): Unit = {
    val ci    = wordsFirstChar(i)
    var ch    = characters(ci)
    var pred  = pred0
    if (pred == null) {
      head = ch
    } else {
      pred.succ = ch
    }
    var j = i + 1
    val n = characters.length
    while (j < n && ch.info.c != ' ') {
      pred      = ch
      ch        = characters(j)
      pred.succ = ch
      j        += 1
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
    }
  }

  def step(): Unit = {
    var curr = head
    if (curr == null) {
      popWord(0, null)
      curr = head
    }

    while (curr != null) {
      curr = curr.succ
    }
  }
}