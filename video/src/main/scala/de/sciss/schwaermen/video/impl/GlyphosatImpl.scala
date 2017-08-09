/*
 *  GlyphosatImpl.scala
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
package impl

import de.sciss.schwaermen.video.Glyphosat.{CharInfo, CharVertex, Word}

import scala.collection.Map
import scala.swing.Graphics2D

final class GlyphosatImpl(charShapes      : Map[Char        , CharInfo],
                          charPairSpacing : Map[(Char, Char), Double  ],
                          words: Array[Word], characters: Array[CharInfo],
                          NominalVX: Float, EjectVY: Float, PairLYK: Float, PairRYK: Float,
                          initialIndex: Int)
  extends Glyphosat {

  //  private[this] val vertexPool =   ...

  @volatile
  private[this] var _head: CharVertex = _

  @volatile
  private[this] var _last: CharVertex = _

  @volatile
  private[this] var _lastWord: CharVertex = _

  private[this] val spaceChar = charShapes(' ')

  private[this] val NominalY    = 512f // 80f // 16f
  private[this] val NominalYK   = 0.01f
  private[this] val NominalVXK  = 0.1f
  private[this] val PairLXK     = 0.15f
  private[this] val EjectXK     = 0.01f
  private[this] val EjectVYK    = 0.2f
  private[this] val PairRXK     = 0.05f
  //  private[this] val PairXL      = 0.0f // spaceChar.bounds.getWidth.toFloat
  private[this] val DragMX      = 1.0f - 0.2f // 0.1f
  private[this] val DragMY      = 1.0f - 0.2f // 0.1f
  //  private[this] val PairYL      = 0.0f
  private[this] val ScreenWidth = 1024f // 400f

  def head    : CharVertex = _head
  def last    : CharVertex = _last
  def lastWord: CharVertex = _lastWord

  def numWords: Int = words.length

  private def popWord(wordIndex: Int, pred0: CharVertex): Unit = {
    val word  = words(wordIndex)
    val ch0   = characters(word.charIndices(0))
    val v0    = new CharVertex(ch0, wordIndex = wordIndex) // XXX TODO --- could avoid allocation
    var pred  = pred0
    if (pred == null) {
      _head     = v0
      v0.x      = ScreenWidth
      v0.y      = NominalY
    } else {
      pred.succ = v0
      v0.x      = math.max(ScreenWidth, pred.x + pred.info.right)
      v0.y      = pred.y
      v0.vy     = pred.vy
    }
    var wi = 1
    pred = v0
    while (wi <= word.length) {
      val ch    = if (wi < word.length) characters(word.charIndices(wi)) else spaceChar
      val v     = new CharVertex(ch, wordIndex = wordIndex)
      pred.succ = v
      v.x       = pred.x + pred.info.right
      // v.x    = math.max(-100, math.min(v.x))
      v.y       = pred.y
      v.vy      = pred.vy
      pred      = v
      wi       += 1
    }

    _lastWord = v0
    _last     = pred
  }

  def render(g: Graphics2D): Unit = {
    var curr = _head
    var px = 0f
    var py = 0f
    while (curr != null) {
      g.translate(curr.x - px, curr.y - py)
      g.fill(curr.info.shape)
      px = curr.x
      py = curr.y
      curr = curr.succ
      // if (curr == _head) sys.error("CYCLIC")
    }
  }

  def step(): Unit = {
    var curr = _head
    if (curr == null) {
      popWord(initialIndex, null)
      curr = _head
    }

    // ---- updates velocities ----

    curr.vx += (NominalVX - curr.vx) * NominalVXK
    curr.vy += (NominalY  - curr. y) * NominalYK
    curr.vx *= DragMX
    curr.vy *= DragMY
    var pred = curr
    curr = curr.succ

    var isSym = false

    while (curr != null) {
      val y2 = curr.y
      val x2 = curr.x + pred.info.left
      curr.vx *= DragMX
      curr.vy *= DragMY
      if (curr.eject) {
        val dx = (ScreenWidth/2) - x2 // XXX TODO --- should take word width into account
        curr.vx += dx * EjectXK                       // (NominalVX - curr.vx) * NominalVXK
        curr.vy += (EjectVY - curr.vy) * EjectVYK
        isSym = true

      } else {
        val x1 = pred.x + pred.info.right
        val y1 = pred.y
        val dx = x1 - x2
        val dy = y1 - y2
        curr.vx += dx * PairLXK
        curr.vy += dy * PairLYK
        if (isSym) {
          pred.vx -= dx * PairRXK
          pred.vy -= dy * PairRYK
        } else {
          isSym = true
        }
      }
      pred = curr
      curr = curr.succ
    }

    // ---- update positions ----

    curr = _head
    pred = curr
    var allOut  = true
    var predIdx = -1
    while (curr != null) {
      if      (curr.vx < -4f) curr.vx = -4f
      else if (curr.vx >  4f) curr.vx =  4f

      if      (curr.vy < -4f) curr.vy = -4f
      else if (curr.vy >  4f) curr.vy =  4f

      curr.x += curr.vx
      curr.y += curr.vy
      if (allOut) {
        if (curr.wordIndex != predIdx) {
          _head   = curr // drop previous head
          predIdx = curr.wordIndex
        }
        val currOut = curr.x + curr.info.right <= 0 || curr.y + curr.info.bottom <= 0
        allOut &= currOut
      }
      pred = curr
      curr = curr.succ
    }

    if (pred.x + pred.info.right < ScreenWidth) {
      popWord((pred.wordIndex + 1) % words.length, pred)
    }
  }
}