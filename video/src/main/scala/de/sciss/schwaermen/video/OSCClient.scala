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

import de.sciss.kollflitz.Vec
import de.sciss.osc
import de.sciss.osc.UDP

import scala.concurrent.stm.{InTxn, Txn, atomic}
import scala.util.Try

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
final class OSCClient(override val config: Config, val dot: Int,
                      val transmitter : UDP.Transmitter .Undirected,
                      val receiver    : UDP.Receiver    .Undirected)
  extends OSCClientLike {

  override def main: Main.type = Main

  private[this] val otherVideos: Vec[SocketAddress] =
    Network.videoSocketSeq.filterNot(_ == transmitter.localSocketAddress)

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Scene.OscInjectQuery(uid) =>
      atomic { implicit tx =>
        Scene.current().queryInjection(sender, uid)
      }

    case _ =>
      oscFallback(p, sender)
  }

  def aliveVideos(): Vec[SocketAddress] = filterAlive(otherVideos)

  def queryVideos[A](m: osc.Message)
                    (handler: PartialFunction[osc.Packet, A])
                    (result: InTxn => Try[List[QueryResult[A]]] => Unit)
                    (implicit tx: InTxn): Unit = {
    val sq  = aliveVideos()
    val q   = new Query[A](this, sq, m, result, handler, tx)
    addQuery(q)
  }

  def queryTxn[A](target: SocketAddress, m: osc.Message)
                 (handler: PartialFunction[osc.Packet, A])
                 (result: InTxn => Try[QueryResult[A]] => Unit)
                 (implicit tx: InTxn): Unit = {
    val sq  = Vector(target)
    val q   = new Query[A](this, sq, m, tx => seq => result(tx)(seq.map(_.head)), handler, tx)
    addQuery(q)
  }

  def sendVideos(m: osc.Message)(implicit tx: InTxn): Unit = {
    val sq = aliveVideos()
    Txn.afterCommit { _ =>
      sq.foreach { target =>
        transmitter.send(m, target)
      }
    }
  }

//  init()
}