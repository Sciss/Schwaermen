/*
 *  Spk.scala
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

import scala.collection.mutable
import scala.util.control.NonFatal

object Spk {
  lazy val default: Spk.Network = {
    val is = getClass.getResourceAsStream("speakers.txt")
    try {
      val arr = new Array[Byte](is.available())
      is.read(arr)
      val s = new String(arr, "UTF-8")
      parse(s)
    } finally {
      is.close()
    }
  }

  def parse(s: String): Spk.Network = {
    var exits   = Map.empty[Int, Int]
    val nMap    = mutable.Map.empty[Int, mutable.ArrayBuilder[Short]]
    val speakers0 = s.split("\n").iterator
      .map { ln =>
        val i = ln.indexOf('#')
        if (i < 0) ln.trim else ln.substring(0, i).trim
      }
      .filter(!_.isEmpty)
      .zipWithIndex.map { case (ln, spkIdx) =>
        val arr = ln.split(":")
        if (arr.length != 3 && arr.length != 4) sys.error(s"Could not parse line '$ln'")
        val id    = arr(0).trim().toShort
        val node  = arr(1).split(",")
        if (node.length != 2) sys.error(s"Could not parse node '$node' in line '$ln'")
        val dot   = node(0).trim().toByte
        val ch    = node(1).trim().toByte
        val neigh = arr(2).split(",")
        try {
          neigh.foreach { nIdS =>
            val t = nIdS.trim()
            if (!t.isEmpty) {
              val nId = t.toShort
              val b1 = nMap.getOrElseUpdate(id , Array.newBuilder)
              val b2 = nMap.getOrElseUpdate(nId, Array.newBuilder)
              b1 += nId
              b2 += id
            }
          }
        } catch {
          case NonFatal(ex) =>
            println(s"In line '$ln'")
            throw ex
        }

        val exit  = if (arr.length > 3) {
          val value = arr(3).trim().toInt
          exits += (value -> spkIdx)
          value.toByte
        } else (-1).toByte
        Spk(id = id, dot = dot, ch = ch, exit = exit, neighbours = null)
      }
      .toArray

    val idMap     = speakers0.iterator.map(_.id).zipWithIndex.toMap
    val speakers  = speakers0.map { spk =>
      spk.copy(neighbours = nMap(spk.id).result().map(nId => idMap(nId).toShort))
    }

    new Spk.Network(speakers, exits)
  }

  /** @param exits  maps speaker _ids_ to exit codes */
  final class Network(val speakers: Array[Spk], exits: Map[Int, Int])
}

/** Speaker identification
  *
  * @param id         logical id, unique among all speakers
  * @param dot        dot of the audio node
  * @param ch         channel within the audio node
  * @param exit       'exit' video-id, -1 for no exit, or 3 for cul-de-sac
  * @param neighbours neighbouring speaker _indices_ (not ids!)
  */
final case class Spk(id: Short, dot: Byte, ch: Byte, exit: Byte, neighbours: Array[Short])