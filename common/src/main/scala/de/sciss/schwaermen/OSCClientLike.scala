/*
 *  OSCClientLike.scala
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

import java.net.SocketAddress
import java.nio.ByteBuffer

import de.sciss.osc
import de.sciss.osc.UDP

abstract class OSCClientLike {
  def tx: UDP.Transmitter .Undirected
  def rx: UDP.Receiver    .Undirected

  def config: ConfigLike

  protected def oscReceived(p: osc.Packet, sender: SocketAddress): Unit

  /** Sends to all possible targets. */
  final def ! (p: osc.Packet): Unit =
    Network.socketSeq.foreach { target =>
      tx.send(p, target)
    }

  final def dumpOSC(): Unit = {
    tx.dump(filter = Network.oscDumpFilter)
    rx.dump(filter = Network.oscDumpFilter)
  }

  private[this] var updater = Option.empty[UpdateTarget]

  final protected def oscUpdateSet(sender: SocketAddress, uid: Int, off: Long, bytes: ByteBuffer): Unit =
    updater.fold[Unit] {
      tx.send(Network.oscUpdateError(uid, "missing /update-init"), sender)
    } { u =>
      if (u.uid == uid) {
        if (u.sender != sender) {
          tx.send(Network.oscUpdateError(uid, "changed sender"), sender)
        } else {
          u.write(off, bytes)
        }
      } else {
        tx.send(Network.oscUpdateError(uid, s"no updater for uid $uid"), sender)
      }
    }

  final protected def oscUpdateInit(sender: SocketAddress, uid: Int, size: Long): Unit = {
    val u = new UpdateTarget(uid, this, sender, size)
    updater.foreach(_.dispose())
    updater = Some(u)
    u.begin()
  }

  final protected def init(): Unit = {
    rx.action = oscReceived
    if (config.dumpOSC) dumpOSC()
    tx.connect()
    rx.connect()
  }
}