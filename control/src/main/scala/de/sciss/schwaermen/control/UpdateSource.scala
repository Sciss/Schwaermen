/*
 *  UpdateSource.scala
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
package control

import java.io.RandomAccessFile
import java.nio.ByteBuffer

import de.sciss.file._
import de.sciss.osc

import scala.concurrent.{Future, Promise}

final class UpdateSource(config: Config, c: OSCClient, instance: Status, debFile: File) {
  private[this] var offset  = 0L
  private[this] val raf     = new RandomAccessFile(debFile, "r")
  private[this] val size    = raf.length()
  private[this] val ch      = raf.getChannel
  private[this] val promise = Promise[Unit]
  private[this] val target  = Network.dotToSocketMap(instance.dot)

  def status: Future[Unit] = promise.future

  private def reply(p: osc.Packet): Unit =
    c.tx.send(p, target)

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
  }
}
