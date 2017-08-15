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
import de.sciss.schwaermen.video.Glyphosat.CharVertex

import scala.collection.{Map, Set, breakOut}
import scala.swing.Graphics2D
import scala.util.Random

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

  def apply(config: Config, text: String)(implicit rnd: Random): Glyphosat = {
    val font    = mkFont(/* if (config.smallWindow) config.fontSize/2 else */ config.fontSize)
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
      c -> new CharInfo(c, shape, bounds)
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
    val words: Array[Word]            = textA.split(" ").map { s =>
      var width = 0.0
      val charIndices: Array[Int] = s.map { c =>
        val ci = charIdxMap(c)
        width += characters(ci).bounds.getWidth
        ci
      } (breakOut)
      new Word(width.toFloat, charIndices)
    }

    new impl.GlyphosatImpl(
      charShapes      = charShapes,
      charPairSpacing = charPairSpacing,
      words           = words,
      characters      = characters,
      NominalVX       = -config.textVX,
      EjectVY         = -config.textEjectVY,
      PairLYK         = config.textPairLYK,
      PairRYK         = config.textPairRYK,
      initialIndex    = Util.rand(words.length),
      isSmall         = config.smallWindow
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

  final class CharInfo(val c: Char, val shape: Shape, val bounds: Rectangle2D) {
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

  final class CharVertex(val info: CharInfo, val wordIndex: Int) {
    @volatile
    var  x: Float = 0f  // position x

    @volatile
    var  y: Float = 0f  // position y

    @volatile
    var vx: Float = 0f  // velocity x

    @volatile
    var vy: Float = 0f  // velocity y
//    var ax: Float = 0f  // acceleration x
//    var ay: Float = 0f  // acceleration y

    @volatile
    var succ: CharVertex = _

    @volatile
    var eject: Boolean = false
  }

  final class Word(val width: Float, val charIndices: Array[Int]) {
    val length: Int = charIndices.length
  }
}
trait Glyphosat {
  def step(): Unit

  def render(g: Graphics2D): Unit

  def head    : CharVertex
  def last    : CharVertex
  def lastWord: CharVertex

  def numWords: Int
}