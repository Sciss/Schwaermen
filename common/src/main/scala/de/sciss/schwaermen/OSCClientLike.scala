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

import de.sciss.osc
import de.sciss.osc.UDP

abstract class OSCClientLike {
  // ---- abstract ----

  def transmitter : UDP.Transmitter .Undirected
  def receiver    : UDP.Receiver    .Undirected

  def config: ConfigLike
  def main  : MainLike

  protected def oscReceived(p: osc.Packet, sender: SocketAddress): Unit

  // ---- impl ----

  private[this] var updater = Option.empty[UpdateTarget]

  final protected def oscFallback(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Network.oscUpdateSet (uid, off, bytes) =>
      updater.fold[Unit] {
        transmitter.send(Network.oscUpdateError(uid, "missing /update-init"), sender)
      } { u =>
        if (u.uid == uid) {
          if (u.sender != sender) {
            transmitter.send(Network.oscUpdateError(uid, "changed sender"), sender)
          } else {
            u.write(off, bytes)
          }
        } else {
          transmitter.send(Network.oscUpdateError(uid, s"no updater for uid $uid"), sender)
        }
      }

    case Network.oscUpdateInit(uid, size) =>
      val u = new UpdateTarget(uid, this, sender, size)
      updater.foreach(_.dispose())
      updater = Some(u)
      u.begin()

    case Network.oscShutdown =>
      if (config.isLaptop)
        println("(laptop) ignoring /shutdown")
      else
        Util.shutdown()

    case Network.oscReboot =>
      if (config.isLaptop)
        println("(laptop) ignoring /reboot")
      else
        Util.reboot()

    case Network.oscQueryVersion =>
      transmitter.send(Network.oscReplyVersion(main.fullVersion), sender)

    case osc.Message("/error", _ @ _*) =>

    case _ =>
      Console.err.println(s"Ignoring unknown OSC packet $p")
      val args = p match {
        case m: osc.Message => m.name +: m.args
        case _: osc.Bundle  => "osc.Bundle" :: Nil
      }
      transmitter.send(osc.Message("/error", "unknown packet" +: args: _*), sender)
  }

  /** Sends to all possible targets. */
  final def ! (p: osc.Packet): Unit =
    Network.soundSocketSeq.foreach { target =>
      transmitter.send(p, target)
    }

  final def dumpOSC(): Unit = {
    transmitter.dump(filter = Network.oscDumpFilter)
    receiver.dump(filter = Network.oscDumpFilter)
  }

  final protected def init(): Unit = {
    receiver.action = oscReceived
    if (config.dumpOSC) dumpOSC()
    transmitter.connect()
    receiver.connect()
  }
}