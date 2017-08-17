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
import de.sciss.schwaermen.video.Scene.OscInjectReply

import scala.concurrent.stm.{InTxn, Txn, atomic}
import scala.util.{Random, Try}

object OSCClient {
  def apply(config: Config, localSocketAddress: InetSocketAddress)
           (implicit rnd: Random): OSCClient = {
    val c                 = UDP.Config()
    c.codec               = Network.oscCodec
    val dot               = Network.resolveDot(config, localSocketAddress)
    c.localSocketAddress  = localSocketAddress
    println(s"OSCClient local socket $localSocketAddress - dot $dot")
    val tx                = UDP.Transmitter(c)
    val rx                = UDP.Receiver(tx.channel, c)
    new OSCClient(config, dot, tx, rx)
  }
}
/** Undirected pair of transmitter and receiver, sharing the same datagram channel. */
final class OSCClient(override val config: Config, val dot: Int,
                      val transmitter : UDP.Transmitter .Undirected,
                      val receiver    : UDP.Receiver    .Undirected,
                     )(implicit rnd: Random)
  extends OSCClientLike {

  override def main: Main.type = Main

  val videoId: Int = {
    if (config.videoId >= 0) config.videoId else {
      val res = Network.videoDotSeq.indexOf(dot)
      if (res >= 0) res else {
        Console.err.println(s"WARNING: No dedicated video text for $dot. Using first instead")
        0
      }
    }
  }

  val allVertices: Array[Array[Vertex]] = Array.tabulate(3)(Vertex.tryReadVertices)

  val speakers: Spk.Network = Spk.readOrDefault(config.speakerPaths)

  private[this] val metaSeq = Array.tabulate(3) { thatId =>
    if (thatId == videoId) null
    else {
      val id1 = math.min(thatId, videoId)
      val id2 = math.max(thatId, videoId)
      TextPathFinder.tryRead(textId1 = id1, textId2 = id2, vertices1 = allVertices(id1), vertices2 = allVertices(id2))
    }
  }

  private[this] val otherVideoNodes: Vec[SocketAddress] = {
    val seqRaw = if (config.otherVideoSockets.nonEmpty)
      config.otherVideoSockets.valuesIterator.toVector
    else
      Network.videoSocketSeq

    seqRaw.filterNot(_ == transmitter.localSocketAddress)
  }

  private[this] val soundNodeMap: Map[Int, SocketAddress] = {
    if (config.soundSockets.nonEmpty) config.soundSockets else {
      (Network.soundDotSeq zip Network.soundSocketSeq).toMap
    }
  }

  def soundNode(dot: Int): Option[SocketAddress] = soundNodeMap.get(dot)

  val vertices: Array[Vertex] = allVertices(videoId)

  override protected val socketSeqCtl: Vec[SocketAddress] =
    if (config.otherVideoSockets.nonEmpty)
      config.otherVideoSockets.valuesIterator.toVector
    else
      Network.socketSeqCtl

  override val socketToDotMap: Map[SocketAddress, Int] =
    if (config.otherVideoSockets.nonEmpty)
      config.otherVideoSockets.map(_.swap)
    else
      Network.socketToDotMap

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Scene.OscInjectQuery(uid, ejectVideoId, ejectVertex) =>
      val meta = metaSeq(ejectVideoId)
      if (meta == null) {
        sendNow(Scene.OscInjectReply(uid, OscInjectReply.Rejected), sender)
      } else
        atomic { implicit tx =>
          Scene.current().queryInjection(sender,
            uid = uid, meta = meta, ejectVideoId = ejectVideoId, ejectVertex = ejectVertex)
        }

    case m @ osc.Message("/test-path-finder") =>
      metaSeq.find(_ != null).foreach { meta =>
        val t1  = System.currentTimeMillis()
        val v1  =  rnd.nextInt(meta.textLen1)                 .toShort
        val v2  = (rnd.nextInt(meta.textLen2) + meta.textLen1).toShort
        meta.finder.findExtendedPath(sourceVertex = v1, targetVertex = v2, pathLen = meta.finder.maxPathLen)
        val t2 = System.currentTimeMillis()
        val dt = t2 - t1
        println(s"$m -- took ${dt}ms")
        sendNow(osc.Message("/test-path-reply", dt), sender)
      }

    case _ =>
      oscFallback(p, sender)
  }

  def aliveVideos(): Vec[SocketAddress] = filterAlive(otherVideoNodes)

  def queryVideos[A](m: osc.Message, extraDelay: Long = 0L)
                    (handler: PartialFunction[osc.Packet, A])
                    (result: InTxn => Try[List[QueryResult[A]]] => Unit)
                    (implicit tx: InTxn): Unit = {
    val sq  = aliveVideos()
    val q   = new Query[A](this, sq, m, result, handler,
      extraDelay = extraDelay, tx0 = tx)
    addQuery(q)
  }

  def queryTxn[A](target: SocketAddress, m: osc.Message, extraDelay: Long = 0L)
                 (handler: PartialFunction[osc.Packet, A])
                 (result: InTxn => Try[QueryResult[A]] => Unit)
                 (implicit tx: InTxn): Unit = {
    val sq  = Vector(target)
    val q   = new Query[A](this, sq, m, tx => seq => result(tx)(seq.map(_.head)), handler,
      extraDelay = extraDelay, tx0 = tx)
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