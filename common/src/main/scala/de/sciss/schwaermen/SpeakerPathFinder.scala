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

import java.{util => ju}

import scala.util.Random

//object SpeakerPathFinder {
//
//}
final class SpeakerPathFinder(network: Spk.Network, maxPathLen: Int = Spk.DefaultMaxPathLen) {
  private[this] val path    = new Array[Short](maxPathLen)
  private[this] val visited = new Array[Short](network.length)

  private def zeroPos(vi: Short, n: Int): Int = {
    var res = 0
    var sh  = vi & 0xFFFF
    var m   = n
    while (true) {
      if ((sh & 1) == 0) {
        if (m == 0) return res
        m -= 1
      }
      res += 1
      sh >>>= 1
    }
    -1 // never here
  }

  /** @param sourceVertex   speaker _index_ of starting point
    * @param targetVertex   speaker _index_ of end point
    */
  def findPath(sourceVertex: Short, targetVertex: Short)(implicit rnd: Random): Array[Spk] = {
    ju.Arrays.fill(visited, 0.toShort)

    import network.speakers

    var pathIdx     = 0
    var currVertexI = sourceVertex
    var currVertex  = speakers(sourceVertex)
    path(0)         = sourceVertex
    val maxPathLenM = maxPathLen - 1

    while (true) {
      val vi                = visited(currVertexI)
      val numVisited        = java.lang.Integer.bitCount(vi)
      val numUnseen         = currVertex.neighbours.length - numVisited

      if (numUnseen == 0 || pathIdx == maxPathLenM) {
        // backtrack
        if (pathIdx == 0) {
          Console.err.println(s"Warning: cannot find speaker path from $sourceVertex to $targetVertex with maxPathLen = $maxPathLen")
          return Array.empty
        }
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
        if (targetI == targetVertex) {
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