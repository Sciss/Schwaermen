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

import de.sciss.schwaermen.video.Glyphosat.{CharInfo, CharVertex, EjectionCandidate, Ejector, Word}
import de.sciss.schwaermen.video.Main.log

import scala.collection.Map
import scala.swing.Graphics2D

final class GlyphosatImpl(charShapes      : Map[Char        , CharInfo],
                          charPairSpacing : Map[(Char, Char), Double  ],
                          words           : Array[Word],
                          val vertices    : Array[Vertex],
                          characters      : Array[CharInfo],
                          NominalVX       : Float,
                          EjectVY         : Float,
                          PairLYK         : Float,
                          PairRYK         : Float,
                          initialIndex    : Int,
                          isSmall         : Boolean
                         )
  extends Glyphosat {

  //  private[this] val vertexPool =   ...

  @volatile
  private[this] var _head: CharVertex = _

//  @volatile
//  private[this] var _last: CharVertex = _
//
//  @volatile
//  private[this] var _lastWord: CharVertex = _

  private[this] val spaceChar       = charShapes(' ')
  private[this] val spaceCharWidth  = spaceChar.width

  private[this] val NominalY    = 512f // if (isSmall) 256f else 512f // 80f // 16f
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
  private[this] val ScreenWidth = 1024f // if (isSmall) 512f else 1024f // 400f
  private[this] val ScreenCX    = ScreenWidth * 0.5f
  private[this] val MinEjectX   = ScreenWidth * 0.75f
  private[this] val MinEjectY   = 48f // 0f

  def head    : CharVertex = _head

//  def last    : CharVertex = _last
//  def lastWord: CharVertex = _lastWord

  def numWords: Int = words.length

  // we measure these with a lag
  private[this] var statVX        = NominalVX * DragMX  // initial guess
  private[this] var statStretchX  = 1.29f               // initial guess

  private[this] val statFpsT    = Array.fill[Long](11)(System.currentTimeMillis())
  private[this] var statFpsIdx  = 0

  /** Current estimation of rendering speed in frames per seconds, taken from measurements */
  def fps: Float = {
    val dt  = statFpsT((statFpsIdx + 10) % 11) - statFpsT(statFpsIdx)  // millis-per-ten-frames
    10000.0f / dt
  }

  private[this] var popWordIndex = initialIndex

  /** Appends a new word to the end of the currently rendered words.
    * Also checks if the newly appearing word is 'ejecting'.
    */
  private def popWord(pred0: CharVertex): Unit = {
    val wordIndex = popWordIndex
    popWordIndex = incWordIndex(wordIndex)
    val word  = words(wordIndex)
    val ch0   = characters(word.charIndices(0))
    val v0    = new CharVertex(ch0, wordIndex = wordIndex) // XXX TODO --- could avoid allocation
    val _ej   = ejectIndex
    if (_ej >= 0 && startWordVertexIdx(word) == _ej) {
      v0.eject = true
    }

    var pred  = pred0
    if (pred == null) {
      v0.x      = ScreenWidth
      v0.y      = NominalY
      _head     = v0
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
      v.x       = pred.right
      // v.x    = math.max(-100, math.min(v.x))
      v.y       = pred.y
      v.vy      = pred.vy
      pred.succ = v
      pred      = v
      wi       += 1
    }

//    _lastWord = v0
//    _last     = pred
  }

  // -1 if it doesn't start a vertex
  private def startWordVertexIdx(w: Word): Int = {
    val indices   = w.vertexIndices
    val vertexIdx = indices(indices.length - 1)
    val v         = vertices(vertexIdx)
    if (v.index == w.peer.index) vertexIdx else -1
  }

  private def lastChar(cv: CharVertex): CharVertex = {
    val wi    = cv.wordIndex
    var curr  = cv
    var pred  = cv
    while (curr != null && curr.wordIndex == wi) {
      pred = curr
      curr = curr.succ
    }
    pred
  }

//  private def nextWord(cv: CharVertex): CharVertex = lastChar(cv).succ

  @volatile
  private[this] var ejectIndex = -1

  @volatile
  private[this] var ejectVisible = false

  @volatile
  private[this] var ejectWordWidth = 0f

  @volatile
  private[this] var ejectDone  = null : Ejector

  def eject(vertexIdx: Int, callBack: Ejector): Unit = {
    ejectDone   = callBack
    ejectIndex  = vertexIdx

    var curr  = _head
    var found = false
    while (curr != null && !found) {
      val w = words(curr.wordIndex)
      if (startWordVertexIdx(w) == vertexIdx) {
        curr.eject  = true
        found       = true
      }
      curr = curr.succ
    }
  }

  def reset(): Unit = {
    ejectDone   = null
    ejectIndex  = -1
    _head       = null
  }

//  private def ejected(): Unit = {
//    ejectIndex    = -1
//    val done      = ejectDone
//    if (done != null) done()
//  }

  /** Returns a vertex-index */
  def ejectionCandidate(delay: Float): EjectionCandidate = {
    // what we want:
    // - we want, with given delay, to find the
    //   earliest word _beginning a vertex_ that
    //   would end up centred on the screen after
    //   the given delay
    //
    // what we do:
    // - based on the parameters and stats,
    //   calculate the estimate of the minimum
    //   current center x position
    // - traverse the current list of words and
    //   find the first word (that starts a
    //   vertex) matching this position. if we
    //   hit the end, virtually append to `succ`
    //
    // Note: vertexIndices has always the smallest
    // vertex index first and largest last. we only have to check the _last_ vertex
    // to determine if the word has the same index and thus is a valid vertex start

    val _fps        = fps
    val delayFrames = delay * _fps
    val absVX       = math.abs(statVX)
    val distance    = delayFrames * absVX
    val minCX       = ScreenCX + distance
    log(f"ejectionCandidate. fps = ${_fps}%1.1f, delayFrames = $delayFrames%1.1f, distance = $distance%1.1f minCX = $minCX%1.1f")


    def test(left: Float, w: Word, ww: Float): EjectionCandidate = {
      val vertexIdx = startWordVertexIdx(w)
      if (vertexIdx < 0) null else {
        val wcx = left + ww * 0.5f
        if (wcx < minCX) null else {
          val wDist       = wcx - ScreenCX
          val wDlyFrames  = wDist / absVX
          val wDly        = wDlyFrames / _fps
          ejectWordWidth  = ww
          EjectionCandidate(vertexIdx, wDly)
        }
      }
    }

    var curr      = _head
    var pred      = curr
    var result    = null: EjectionCandidate
    while (curr != null && result == null) {
      val w     = words(curr.wordIndex)
      val ww    = w.width * statStretchX
      result    = test(left = curr.left, w = w, ww = ww)
      pred      = lastChar(curr)
      curr      = pred.succ
    }
    if (result == null) {
      // now we need to advance with the yet invisible words
      var wordIndex = nextWordIndex(pred)
      val spaceCharStretch = spaceCharWidth * statStretchX
      var currLeft  = if (pred == null) ScreenWidth else pred.right + spaceCharStretch
      while (result == null) {
        val w     = words(wordIndex)
        val ww    = w.width * statStretchX
        result    = test(left = currLeft, w = w, ww = ww)
        wordIndex = incWordIndex(wordIndex)
        currLeft += ww + spaceCharStretch
      }
      ejectVisible    = false
    } else {
      ejectVisible    = true
    }

    result
  }

  private def nextWordIndex(cv: CharVertex): Int =
    if (cv == null) popWordIndex + 1 /* initialIndex */ else incWordIndex(cv.wordIndex)

  @inline
  private[this] def incWordIndex(i: Int): Int = (i + 1) % words.length

  /** Draws the current world state */
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

  /** Advances the world by one frame */
  def step(): Unit = {
    var curr = _head
    if (curr == null) {
      if (ejectVisible) return

      popWord(null)
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
      if (curr.eject && x2 < MinEjectX) {
        val dx        = (ScreenWidth - ejectWordWidth) / 2 - x2
        curr.vx      += dx * EjectXK
        curr.vy      += (EjectVY - curr.vy) * EjectVYK
        ejectVisible  = true
        isSym         = true

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

    def updateStretchStat(v: CharVertex): Unit = {
      val w         = words(v.wordIndex)
      val nominal   = w.width
      val real      = v.right - v.left
      val stretch   = real / nominal
      statStretchX  = statStretchX * 0.9f + stretch * 0.1f
      statVX        = statVX       * 0.9f + v.vx * 0.1f
    }

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

      if (curr.eject && !curr.ejectNotifiedThresh && curr.y <= MinEjectY) {
        curr.ejectNotifiedThresh = true
        val done = ejectDone
        if (done != null) done.ejectWordThresh()
      }

      if (allOut) {
        if (curr.wordIndex != predIdx) {  // i.e. previous word is all out
          // XXX TODO: what was the following guard about?
          // if (_head != curr) {
            updateStretchStat(pred)
          // }

          val wasEject = _head.eject
          _head   = curr // drop previous head
          if (wasEject) {
            curr.eject                = true
            curr.ejectNotifiedThresh  = true
          }
          predIdx = curr.wordIndex
        }
        val currOut = curr.x + curr.info.right <= 0 || curr.y + curr.info.bottom <= 0
        allOut &= currOut
      }
      pred = curr
      curr = curr.succ
    }

    if (pred.x + pred.info.right < ScreenWidth && !ejectVisible) {
      popWord(pred)

    } else if (allOut && pred != null) {
      updateStretchStat(pred)
      _head = null
      val done = ejectDone
      if (done != null) done.ejectAllClear()
    }

    // update fps
    statFpsT(statFpsIdx)  = System.currentTimeMillis()
    statFpsIdx            = (statFpsIdx + 1) % 11
  }
}