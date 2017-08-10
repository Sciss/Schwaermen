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
import java.util.concurrent.TimeoutException

import de.sciss.kollflitz.Vec
import de.sciss.osc
import de.sciss.osc.UDP
import de.sciss.schwaermen.video.OSCClient.Query

import scala.concurrent.stm.{InTxn, Ref, TSet, Txn, atomic}
import scala.util.{Failure, Success, Try}

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

  private final class Query[A](c: OSCClient, sq: Vec[SocketAddress], mOut: osc.Message,
                               result: InTxn => Try[Seq[QueryResult[A]]] => Unit,
                               handler: PartialFunction[osc.Message, A],
                               tx0: InTxn) {

    private[this] val values    = Ref(List.empty[QueryResult[A]])
    private[this] val remaining = TSet(sq: _*)

    private[this] val timeOut = c.schedule(Network.TimeOutMillis) { implicit tx =>
      c.removeQuery(this)
      result(tx)(Failure(new TimeoutException(mOut.name)))
    } (tx0)

    def handle(sender: SocketAddress, mIn: osc.Message)(implicit tx: InTxn): Boolean = {
      remaining.remove(sender) && handler.isDefinedAt(mIn) && {
        val value = handler(mIn)
        val dot   = Network.socketToDotMap.getOrElse(sender, -1)
        val res   = QueryResult(dot, value)
        val vs    = values.transformAndGet(res :: _)
        remaining.isEmpty && {
          timeOut.cancel()
          result(tx)(Success(vs))
          true
        }
      }
    }

    Txn.afterCommit { _ =>
      sq.foreach { target =>
        c.transmitter.send(mOut, target)
      }
    } (tx0)
  }
}
/** Undirected pair of transmitter and receiver, sharing the same datagram channel. */
final class OSCClient(override val config: Config, val dot: Int,
                      val transmitter : UDP.Transmitter .Undirected,
                      val receiver    : UDP.Receiver    .Undirected)
  extends OSCClientLike {

  override def main: Main.type = Main

  private[this] val timer = new java.util.Timer("video-timer")

  private[this] val queries = Ref(List.empty[Query[_]])

  def schedule(delay: Long)(body: InTxn => Unit)(implicit tx: InTxn): Task =
    Task(timer, delay)(body)

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Network.oscHeart =>

    case m: osc.Message =>
      val wasHandled = atomic { implicit tx =>
        val qsIn      = queries()
        val qOpt      = qsIn.find(_.handle(sender, m))
        val _handled  = qOpt.isDefined
        qOpt.foreach(removeQuery(_))
        _handled
      }

      if (!wasHandled) oscFallback(p, sender)

    case _ =>
      oscFallback(p, sender)
  }

  def removeQuery[A](q: Query[A])(implicit tx: InTxn): Unit =
    queries.transform(_.filterNot(_ == q))

  def queryVideos[A](m: osc.Message)
                    (result: InTxn => Try[Seq[QueryResult[A]]] => Unit)
                    (handler: PartialFunction[osc.Message, A])
                    (implicit tx: InTxn): Unit = {
    val sq  = Network.videoSocketSeq
    val q   = new Query[A](this, sq, m, result, handler, tx)
    queries.transform(q :: _)
  }

  init()
}