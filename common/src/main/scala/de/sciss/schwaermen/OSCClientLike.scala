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

import de.sciss.equal.Implicits._
import de.sciss.kollflitz.Vec
import de.sciss.osc
import de.sciss.osc.UDP

import scala.concurrent.stm.{InTxn, Ref, Txn, atomic}

abstract class OSCClientLike {
  // ---- abstract ----

  def transmitter : UDP.Transmitter .Undirected
  def receiver    : UDP.Receiver    .Undirected

  def config: ConfigLike
  def main  : MainLike
  def dot   : Int

  protected def oscReceived(p: osc.Packet, sender: SocketAddress): Unit

  // ---- impl ----

  private[this] var updater = Option.empty[UpdateTarget]

  final val timer = new java.util.Timer("video-timer")

  private[this] val queries = Ref(List.empty[Query[_]])

  @volatile
  private[this] var alive   = Map.empty[SocketAddress, Long]

  final def filterAlive(in: Vec[SocketAddress]): Vec[SocketAddress] =
    in.filter(alive.contains)

  final def scheduleTxn(delay: Long)(body: InTxn => Unit)(implicit tx: InTxn): Task =
    Task(timer, delay)(body)

  final def removeQuery[A](q: Query[A])(implicit tx: InTxn): Unit =
    queries.transform(_.filterNot(_ == /* ===  */ q))

  protected final def addQuery[A](q: Query[A])(implicit tx: InTxn): Unit =
    queries.transform(q :: _)

  final protected def oscFallback(p: osc.Packet, sender: SocketAddress): Unit = {
    val wasHandled = atomic { implicit tx =>
      val qsIn      = queries()
      val qOpt      = qsIn.find(_.handle(sender, p))
      val _handled  = qOpt.isDefined
      qOpt.foreach(removeQuery(_))
      _handled
    }

    if (!wasHandled) p match {
      case Network.OscHeart =>
        val now   = System.currentTimeMillis()
        val death = now + Network.DeathPeriodMillis
        alive = alive.filter(_._2 < death) + (sender -> now)

      case Network.OscUpdateSet (uid, off, bytes) =>
        updater.fold[Unit] {
          transmitter.send(Network.OscUpdateError(uid, "missing /update-init"), sender)
        } { u =>
          if (u.uid === uid) {
            if (u.sender != sender) {
              transmitter.send(Network.OscUpdateError(uid, "changed sender"), sender)
            } else {
              u.write(off, bytes)
            }
          } else {
            transmitter.send(Network.OscUpdateError(uid, s"no updater for uid $uid"), sender)
          }
        }

      case Network.OscUpdateInit(uid, size) =>
        val u = new UpdateTarget(uid, this, sender, size)
        updater.foreach(_.dispose())
        updater = Some(u)
        u.begin()

      case Network.OscShutdown =>
        if (config.isLaptop)
          println("(laptop) ignoring /shutdown")
        else
          Util.shutdown()

      case Network.OscReboot =>
        if (config.isLaptop)
          println("(laptop) ignoring /reboot")
        else
          Util.reboot()

      case Network.OscQueryVersion =>
        transmitter.send(Network.OscReplyVersion(main.fullVersion), sender)

      case osc.Message("/error"        , _ @ _*) =>
      case osc.Message("/inject-abort" , _ @ _*) =>
      case osc.Message("/inject-commit", _ @ _*) =>

      case _ =>
        Console.err.println(s"Ignoring unknown OSC $p")
        val args = p match {
          case m: osc.Message => m.name +: m.args
          case _: osc.Bundle  => "osc.Bundle" :: Nil
        }
        transmitter.send(osc.Message("/error", "unknown packet" +: args: _*), sender)
    }
  }

  /** Sends to all possible targets. */
  final def ! (p: osc.Packet): Unit =
    Network.socketSeqCtl.foreach { target =>
      transmitter.send(p, target)
    }

  final def sendTxn(target: SocketAddress, p: osc.Packet)(implicit tx: InTxn): Unit =
    Txn.afterCommit(_ => transmitter.send(p, target))

  final def dumpOSC(): Unit = {
    transmitter.dump(filter = Network.oscDumpFilter)
    receiver   .dump(filter = Network.oscDumpFilter)
  }

  final def init(): Unit = {
    receiver.action = oscReceived
    if (config.dumpOSC) dumpOSC()
    transmitter.connect()
    receiver.connect()
  }

  private[this] val txnCount = Ref(0)

  final def mkTxnId()(implicit tx: InTxn): Long = {
    val i = txnCount.getAndTransform(_ + 1)
//    (dot.toLong << 32) | i
    (i.toLong * 1000) + dot
  }
}