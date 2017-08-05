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

package de.sciss.schwaermen.sound

import java.net.{InetSocketAddress, SocketAddress}
import java.nio.ByteBuffer

import de.sciss.osc
import de.sciss.osc.UDP

object OSCClient {
  def apply(config: Config, host: String): OSCClient = {
    val c     = UDP.Config()
    c.codec   = osc.PacketCodec().doublePrecision().packetsAsBlobs()
    c.localSocketAddress = new InetSocketAddress(host, Config.ClientPort)
    val tx    = UDP.Transmitter(c)
    val rx    = UDP.Receiver(tx.channel, c)
    new OSCClient(config, tx, rx)
  }

}
/** Undirected pair of transmitter and receiver, sharing the same datagram channel. */
final class OSCClient(config: Config, val tx: UDP.Transmitter.Undirected, val rx: UDP.Receiver.Undirected) {
  /** Sends to all possible targets. */
  def ! (p: osc.Packet): Unit =
    Config.socketSeq.foreach { target =>
      tx.send(p, target)
    }

  def dumpOSC(): Unit = {
    tx.dump()
    rx.dump()
  }

  private[this] var updater = Option.empty[Updater]

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case osc.Message("/update-set", off: Long, bytes: ByteBuffer) =>
      updater.fold[Unit] {
        tx.send(osc.Message("/error", "update", "missing /update-init"), sender)
      } { u =>
        if (u.sender != sender) {
          tx.send(osc.Message("/error", "update", "changed sender"), sender)
        } else {
          u.write(off, bytes)
        }
      }

    case osc.Message("/update-init", size: Long) =>
      val u = new Updater(config, this, sender, size)
      updater.foreach(_.dispose())
      updater = Some(u)
      u.begin()

    case osc.Message("/shutdown") =>
      if (config.isLaptop)
        println("(laptop) ignoring /shutdown")
      else
        Main.shutdown()

    case osc.Message("/reboot"  ) =>
      if (config.isLaptop)
        println("(laptop) ignoring /reboot")
      else
        Main.reboot  ()

    case osc.Message("/query", "version") =>
      tx.send(osc.Message("/info", "version", Main.fullVersion), sender)

    case _ =>
      Console.err.println(s"Ignoring unknown OSC packet $p")
      tx.send(osc.Message("/error", "unknown packet", p), sender)
  }

  rx.action = oscReceived
  if (config.dumpOSC) dumpOSC()
  tx.connect()
  rx.connect()
}