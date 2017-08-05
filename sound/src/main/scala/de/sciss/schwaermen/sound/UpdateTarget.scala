/*
 *  UpdateTarget.scala
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
package sound

import java.io.RandomAccessFile
import java.net.SocketAddress
import java.nio.ByteBuffer

import de.sciss.file._
import de.sciss.osc

final class UpdateTarget(config: Config, c: OSCClient, val sender: SocketAddress, size: Long) {
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
    reply(Network.oscUpdateGet(offset))

  def write(off: Long, bytes: ByteBuffer): Unit = {
    if (off != offset) {
      reply(Network.oscUpdateError(s"expected offset $offset but got $off"))
      queryNext()
    } else {
      val plus = bytes.remaining()
      ch.write(bytes)
      offset += plus
      if (offset < size) queryNext()
      else transferCompleted()
    }
  }

  private def sudo(cmd: String*): Int = {
    import sys.process._
    if (config.isLaptop) {
      Process("sudo" +:"-A" +: cmd, None, "SUDO_ASKPASS" -> "/usr/bin/ssh-askpass").!
    } else {
      Process("sudo" +: cmd).!
    }
  }

  private def transferCompleted(): Unit = {
    import sys.process._
    val resInfo = Seq("dpkg", "--info", f.path).!
    if (resInfo == 0) {
      val resRemove = sudo("apt", "remove", Main.packageName)
      if (resRemove != 0) {
        Console.err.println(s"Warning. apt remove was not successful")
      }
      val resInstall = sudo("dpkg", "--install", f.path)
      dispose()
      val m = if (resInstall == 0) {
        Network.oscUpdateSuccess
      } else {
        Network.oscUpdateError(s"dpkg --install returned $resInstall")
      }
      reply(m)

    } else {
      reply(Network.oscUpdateError(s"dpkg --info returned $resInfo"))
    }
  }

  def dispose(): Unit = {
    ch.close()
    f.delete()
  }
}
