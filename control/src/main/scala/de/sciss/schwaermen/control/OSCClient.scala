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
package control

import java.net.{InetSocketAddress, SocketAddress}

import de.sciss.model.impl.ModelImpl
import de.sciss.osc
import de.sciss.osc.UDP

object OSCClient {
  final val Port = 57110

  def apply(config: Config, host: String): OSCClient = {
    val c                 = UDP.Config()
    c.codec               = Network.oscCodec
    val localSocket       = new InetSocketAddress(host, Port)
    c.localSocketAddress  = localSocket
    println(s"OSCClient local socket $localSocket")
    val tx                = UDP.Transmitter(c)
    val rx                = UDP.Receiver(tx.channel, c)
    new OSCClient(config, tx, rx)
  }

  sealed trait Update
  final case class Added  (status: Status) extends Update
  final case class Removed(status: Status) extends Update
  final case class Changed(status: Status) extends Update
}
/** Undirected pair of transmitter and receiver, sharing the same datagram channel. */
final class OSCClient(config: Config, val tx: UDP.Transmitter.Undirected, val rx: UDP.Receiver.Undirected)
  extends ModelImpl[OSCClient.Update] {

  private[this] val sync = new AnyRef
  private[this] var _instances = Vector.empty[Status]

  def instances: Vector[Status] = sync.synchronized(_instances)

  /** Sends to all possible targets, including laptop itself. */
  def ! (p: osc.Packet): Unit =
    Network.socketSeqCtl.foreach { target =>
      tx.send(p, target)
    }

  def dumpOSC(): Unit = {
    tx.dump()
    rx.dump()
  }

  private[this] var updater = Option.empty[UpdateSource]

  def getDot(sender: SocketAddress): Int = sender match {
    case inet: InetSocketAddress =>
      val arr = inet.getAddress.getAddress
      if (arr.length == 4) arr(3) else -1
    case _ => -1
  }

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Network.oscUpdateGet(off) =>
      updater.foreach { u =>
        ???
//        if (u.sender != sender) {
//          tx.send(osc.Message("/error", "update", "changed sender"), sender)
//        } else {
//          u.write(off, bytes)
//        }
      }

//    case osc.Message("/query", "version") =>
//      tx.send(osc.Message("/info", "version", Main.fullVersion), sender)

    case Network.oscReplyVersion(s) =>
      val dot = getDot(sender)
      if (dot >= 0) {
        val idx = _instances.indexWhere(_.dot == dot)
        if (idx < 0) {
          val pos     = Network.dotSeq.indexOf(dot)
          val status  = Status(pos = pos, dot = dot, version = s, update = 0.0)
          _instances :+= status
          dispatch(OSCClient.Added(status))
        } else {
          val statusOld = _instances(idx)
          if (statusOld.version != s) {
            val statusNew = statusOld.copy(version = s)
            _instances = _instances.updated(idx, statusNew)
            dispatch(OSCClient.Changed(statusNew))
          }
        }
      }

    case _ =>
      Console.err.println(s"Ignoring unknown OSC packet $p")
      tx.send(osc.Message("/error", "unknown packet", p), sender)
  }

  rx.action = oscReceived
  dumpOSC()
  tx.connect()
  rx.connect()
}