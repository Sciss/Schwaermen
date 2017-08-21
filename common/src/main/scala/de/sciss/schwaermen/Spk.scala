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

import java.io.{FileInputStream, InputStream}

import de.sciss.file.File

import scala.collection.mutable
import scala.util.control.NonFatal

object Spk {
  final val DefaultMaxPathLen = 108

  lazy val default: Spk.Network = {
    val is = getClass.getResourceAsStream("speakers.txt")
    try {
      read(is)
    } finally {
      is.close()
    }
  }

  private def read(is: InputStream): Spk.Network = {
    val arr = new Array[Byte](is.available())
    is.read(arr)
    val s = new String(arr, "UTF-8")
    parse(s)
  }

  def read(f: File): Spk.Network = {
    val is = new FileInputStream(f)
    try {
      read(is)
    } finally {
      is.close()
    }
  }

  def readOrDefault(f: Option[File]): Spk.Network = f.fold(default)(read)

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
        val ch    = (node(1).trim().toByte - 1).toByte // 1-based in the text, 0-based in the API
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
      val n = nMap(spk.id).result().map { nId =>
        require (nId != spk.id, s"Self cycle for $nId")
        idMap(nId).toShort
      }
      spk.copy(neighbours = n)
    }

    new Spk.Network(speakers, exits)
  }

  /** @param exits  maps exit codes to speaker _indices_ */
  final class Network(val speakers: Array[Spk], val exits: Map[Int, Int]) {
    def length: Int = speakers.length
  }
}

/** Speaker identification
  *
  * @param id         logical id, unique among all speakers
  * @param dot        dot of the audio node
  * @param ch         channel within the audio node (zero-based)
  * @param exit       'exit' video-id, -1 for no exit, or 3 for cul-de-sac
  * @param neighbours neighbouring speaker _indices_ (not ids!)
  */
final case class Spk(id: Short, dot: Byte, ch: Byte, exit: Byte, neighbours: Array[Short]) {
  def canOverlap(that: Spk): Boolean = this.dot != that.dot || (this.ch / 6) != (that.ch / 6)
}