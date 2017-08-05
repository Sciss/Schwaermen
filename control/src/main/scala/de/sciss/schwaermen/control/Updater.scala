/*
 *  Updater.scala
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

package de.sciss.schwaermen.control

import java.io.RandomAccessFile
import java.net.SocketAddress
import java.nio.ByteBuffer

import de.sciss.file._
import de.sciss.osc

final class Updater(config: Config, c: OSCClient, val sender: SocketAddress, size: Long) {
  private[this] var offset = 0L
  private[this] val f     = File.createTemp(suffix = ".deb")
  private[this] val raf   = new RandomAccessFile(f, "rw")
  private[this] val ch    = raf.getChannel

  private def reply(p: osc.Packet): Unit =
    c.tx.send(p, sender)

  def begin(): Unit = {
    require(offset == 0L)
    queryNext()
  }

  private def queryNext(): Unit =
    reply(osc.Message("/update-get", offset))

  def write(off: Long, bytes: ByteBuffer): Unit = {
    if (off != offset) {
      reply(osc.Message("/error", s"expected offset $offset but got $off"))
      queryNext()
    } else {
      val plus = bytes.remaining()
      ch.write(bytes)
      offset += plus
      if (offset < size) queryNext()
      else ??? // transferCompleted()
    }
  }

  def dispose(): Unit = {
    ch.close()
    f.delete()
  }
}
