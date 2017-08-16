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
import scala.util.{Random, Try}

object OSCClient {
  def apply(config: Config, localSocketAddress: InetSocketAddress, meta: PathFinder.Meta)
           (implicit rnd: Random): OSCClient = {
    val c                 = UDP.Config()
    c.codec               = Network.oscCodec
    val dot = if (config.dot >= 0) config.dot else {
      val dot0 = Network.socketToDotMap.getOrElse(localSocketAddress, -1)
      val res = if (dot0 >= 0) dot0 else {
        localSocketAddress.getAddress.getAddress.last.toInt
      }
      if (dot0 < 0) println(s"Warning - could not determine 'dot' for host $localSocketAddress - assuming $res")
      res
    }
    c.localSocketAddress  = localSocketAddress
    println(s"OSCClient local socket $localSocketAddress")
    val tx                = UDP.Transmitter(c)
    val rx                = UDP.Receiver(tx.channel, c)
    new OSCClient(config, dot, tx, rx, meta = meta)
  }
}
/** Undirected pair of transmitter and receiver, sharing the same datagram channel. */
final class OSCClient(override val config: Config, val dot: Int,
                      val transmitter : UDP.Transmitter .Undirected,
                      val receiver    : UDP.Receiver    .Undirected,
                      val meta        : PathFinder.Meta
                     )(implicit rnd: Random)
  extends OSCClientLike {

  override def main: Main.type = Main

  private[this] val otherVideos: Vec[SocketAddress] = {
    val seqRaw = if (config.otherVideoSockets.nonEmpty) config.otherVideoSockets else Network.videoSocketSeq
    seqRaw.filterNot(_ == transmitter.localSocketAddress)
  }

  override protected val socketSeqCtl: Vec[SocketAddress] =
    if (config.otherVideoSockets.nonEmpty) config.otherVideoSockets
    else Network.socketSeqCtl

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Scene.OscInjectQuery(uid, ejectVideoId, ejectVertex) =>
      atomic { implicit tx =>
        Scene.current().queryInjection(sender,
          uid = uid, meta = meta, ejectVideoId = ejectVideoId, ejectVertex = ejectVertex)
      }

    case m @ osc.Message("/test-path-finder") =>
      val t1  = System.currentTimeMillis()
      val v1  =  rnd.nextInt(meta.textLen1)                 .toShort
      val v2  = (rnd.nextInt(meta.textLen2) + meta.textLen1).toShort
      meta.finder.findExtendedPath(sourceVertex = v1, targetVertex = v2, pathLen = meta.finder.maxPathLen)
      val t2 = System.currentTimeMillis()
      val dt = t2 - t1
      println(s"$m -- took ${dt}ms")
      transmitter.send(osc.Message("/test-path-reply", dt), sender)

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