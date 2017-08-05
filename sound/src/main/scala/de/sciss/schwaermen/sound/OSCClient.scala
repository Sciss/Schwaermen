/*
 *  OSCClient.scala
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

import java.net.{InetSocketAddress, SocketAddress}

import de.sciss.osc
import de.sciss.osc.UDP

object OSCClient {
  def apply(config: Config, host: String): OSCClient = {
    val c                 = UDP.Config()
    c.codec               = Network.oscCodec
    val localSocket       = new InetSocketAddress(host, Network.ClientPort)
    c.localSocketAddress  = localSocket
    println(s"OSCClient local socket $localSocket")
    val tx                = UDP.Transmitter(c)
    val rx                = UDP.Receiver(tx.channel, c)
    new OSCClient(config, tx, rx)
  }

}
/** Undirected pair of transmitter and receiver, sharing the same datagram channel. */
final class OSCClient(config: Config, val tx: UDP.Transmitter.Undirected, val rx: UDP.Receiver.Undirected) {
  /** Sends to all possible targets. */
  def ! (p: osc.Packet): Unit =
    Network.socketSeq.foreach { target =>
      tx.send(p, target)
    }

  def dumpOSC(): Unit = {
    tx.dump()
    rx.dump()
  }

  private[this] var updater = Option.empty[UpdateTarget]

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Network.oscUpdateSet(off, bytes) =>
      updater.fold[Unit] {
        tx.send(Network.oscUpdateError("missing /update-init"), sender)
      } { u =>
        if (u.sender != sender) {
          tx.send(Network.oscUpdateError("changed sender"), sender)
        } else {
          u.write(off, bytes)
        }
      }

    case Network.oscUpdateInit(size) =>
      val u = new UpdateTarget(config, this, sender, size)
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
        Util.reboot  ()

    case Network.oscQueryVersion =>
      tx.send(Network.oscReplyVersion(Main.fullVersion), sender)

    case _ =>
      Console.err.println(s"Ignoring unknown OSC packet $p")
      tx.send(osc.Message("/error", "unknown packet", p), sender)
  }

  rx.action = oscReceived
  if (config.dumpOSC) dumpOSC()
  tx.connect()
  rx.connect()
}