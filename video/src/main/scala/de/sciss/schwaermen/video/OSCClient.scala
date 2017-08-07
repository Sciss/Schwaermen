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
package video

import java.net.{InetSocketAddress, SocketAddress}

import de.sciss.osc
import de.sciss.osc.UDP

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
final class OSCClient(override val config: Config, val dot: Int, val tx: UDP.Transmitter.Undirected,
                      val rx: UDP.Receiver.Undirected) extends OSCClientLike {

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
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
      tx.send(Network.oscReplyVersion(Main.fullVersion), sender)

    case Network.oscHeart =>

    case _ =>
      Console.err.println(s"Ignoring unknown OSC packet $p")
      val args = p match {
        case m: osc.Message => m.name +: m.args
        case _: osc.Bundle  => "osc.Bundle" :: Nil
      }
      tx.send(osc.Message("/error", "unknown packet" +: args: _*), sender)
  }

  init()
}