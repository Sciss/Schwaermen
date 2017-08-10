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

import scala.util.control.NonFatal

object OSCClient {
  def apply(config: Config, host: String): OSCClient = {
    val c                 = UDP.Config()
    c.codec               = Network.oscCodec
    val localSocket       = new InetSocketAddress(host, Network.ClientPort)
    val dot               = Network.socketToDotMap.getOrElse(localSocket, -1)
    if (dot < 0) println(s"Warning - could not determine 'dot' for host $host")
    c.localSocketAddress  = localSocket
    println(s"OSCClient local socket $localSocket")
    val tx                = UDP.Transmitter(c)
    val rx                = UDP.Receiver(tx.channel, c)
    new OSCClient(config, dot, tx, rx)
  }

}
/** Undirected pair of transmitter and receiver, sharing the same datagram channel. */
final class OSCClient(override val config: Config, val dot: Int, val transmitter: UDP.Transmitter.Undirected,
                      val receiver: UDP.Receiver.Undirected) extends OSCClientLike {
  val relay: RelayPins  = RelayPins.map(dot)

  override def main: Main.type = Main

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case osc.Message("/test-pin-mode") =>
      try {
        relay.bothPins
        transmitter.send(osc.Message("/done", "test-pin-mode"), sender)
      } catch {
        case NonFatal(ex) =>
          transmitter.send(osc.Message("/fail", "test-pin-mode", ex.toString), sender)
      }

    case osc.Message("/test-channel", ch: Int) =>
      try {
        relay.selectChannel(ch)
        transmitter.send(osc.Message("/done", "test-channel", ch), sender)
      } catch {
        case NonFatal(ex) =>
          transmitter.send(osc.Message("/fail", "test-channel", ch, ex.toString), sender)
      }

    case _ =>
      oscFallback(p, sender)
  }

  init()
}