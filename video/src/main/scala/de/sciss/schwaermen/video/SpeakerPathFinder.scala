/*
 *  SpeakerPathFinder.scala
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

import scala.util.Random
import java.{util => ju}

//object SpeakerPathFinder {
//
//}
final class SpeakerPathFinder(network: Spk.Network, maxPathLen: Int) {
  private[this] val path    = new Array[Short](maxPathLen)
  private[this] val visited = new Array[Short](network.length)

  private def zeroPos(vi: Short, n: Int): Int = {
    var res = 0
    var sh  = vi & 0xFFFF
    while ((sh & 1) == 0) {
      res += 1
      sh >>>= 1
    }
    res
  }

  /** @param v1   speaker _index_ of starting point
    * @param v2   speaker _index_ of end point
    */
  def perform(v1: Short, v2: Short)(implicit rnd: Random): Array[Spk] = {
    ju.Arrays.fill(visited, 0.toShort)

    import network.speakers

    var pathIdx     = 0
    var currVertexI = v1
    var currVertex  = speakers(v1)
    path(0)         = v1

    while (true) {
      val vi                = visited(currVertexI)
      val numVisited        = java.lang.Integer.bitCount(vi)
      val numUnseen         = currVertex.neighbours.length - numVisited

      if (numUnseen == 0) {
        // backtrack
        assert(pathIdx > 0)
        pathIdx -= 1
        currVertexI   = path(pathIdx)
        currVertex    = speakers(currVertexI)

      } else {
        val nIdx      = zeroPos(vi, rnd.nextInt(numUnseen))
        val targetI   = currVertex.neighbours(nIdx)
        visited(currVertexI) = (vi | (1 << nIdx)).toShort
        val target    = speakers(targetI)
        val nIdxT     = {
          var i = 0
          val n = target.neighbours
          var found = false
          while (i < n.length && !found) {
            if (n(i) == currVertexI) found = true
            else i += 1
          }
          assert(found)
          i
        }
        visited(targetI) = (visited(targetI) | (1 << nIdxT)).toShort

        currVertexI = targetI
        currVertex  = speakers(targetI)
        pathIdx    += 1
        path(pathIdx) = currVertexI
        if (targetI == v2) {
          pathIdx += 1
          val res = new Array[Spk](pathIdx)
          var i = 0
          while (i < pathIdx) {
            res(i) = speakers(path(i))
            i += 1
          }
          return res
        }
      }
    }

    null  // never here
  }
}