/*
 *  TextScene.scala
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

import java.net.SocketAddress

import de.sciss.equal.Implicits._
import de.sciss.schwaermen.video.Main.log
import de.sciss.schwaermen.video.Scene.{OscInjectAbort, OscInjectCommit, OscInjectQuery, OscInjectReply}

import scala.concurrent.stm.{InTxn, Ref}
import scala.swing.Swing
import scala.util.{Failure, Random, Success}

object TextScene {
  private sealed trait State
  private case object Idle          extends State
//  private case object Busy          extends State
  private case object InjectQuery   extends State
  private case object InjectPending extends State
  private case object Ejecting      extends State
  private case object Injecting     extends State
}
final class TextScene(c: OSCClient)(implicit rnd: Random) extends Scene.Text {
  import TextScene._

  private[this] val config = c.config

  private[this] val stateRef    = Ref[State](Idle)
  private[this] val idleMinTime = Ref(0L)

  private[this] val videoId: Int = {
    if (config.videoId >= 0) config.videoId else {
      val res = Network.videoDotSeq.indexOf(c.dot)
      if (res >= 0) res else {
        Console.err.println(s"WARNING: No dedicated video text for ${c.dot}. Using first instead")
        0
      }
    }
  }

  private[this] val text: String = {
    Util.readTextResource(s"text${videoId + 1}.txt").replaceAll("\n", " ——— ") // XXX TODO decide here
  }

  private[this] val gl = Glyphosat(config, text)

//  private[this] lazy val debugView: DebugTextView = new DebugTextView
  private[this] lazy val view: TextView = new TextView(config, gl, videoId = videoId)

  /* Pixels per second */
//  private[this] val textSpeed         : Float = config.textVX * config.fps
//  private[this] val pixelsUntilTimeOut: Float = textSpeed * Network.TimeOutSeconds

  private[this] val idleTask = Ref(Option.empty[Task])

  if (!config.debugText) Swing.onEDT {
    view
  }

  def queryInjection(sender: SocketAddress, Uid: Long)(implicit tx: InTxn): Unit = {
    if (stateRef() == Idle) {
      stateRef() = InjectPending
      val reply = OscInjectReply(Uid, OscInjectReply.Accepted)
      c.queryTxn(sender, reply) {
        case OscInjectCommit(Uid, targetDot)  => targetDot === c.dot
        case OscInjectAbort (Uid)             => false
      } { implicit tx => {
        case Success(QueryResult(_ /* dot */, true)) =>
          testInjectAndGoBackToIdle()
        case _ =>
          log("Injection target received abortion or was not selected")
          retryInitiative()
      }}
    } else {
      val reply = OscInjectReply(Uid, OscInjectReply.Rejected)
      c.sendTxn(sender, reply)
    }
  }

  private def startInitiative()(implicit tx: InTxn): Unit = {
    idleTask() = None
    if (stateRef() == Idle) {
      stateRef() = InjectQuery
      log("Starting initiative")
      val Uid = c.mkTxnId()
      c.queryVideos(OscInjectQuery(Uid)) {
        case OscInjectReply(Uid, accepted) => accepted
      } { implicit tx => {
        case Success(list) =>
          log(s"Got replies: $list")
          if (list.exists(_.value === OscInjectReply.Rejected)) {
            log("A node rejected the transaction")
            c.sendVideos(OscInjectAbort(Uid))
            retryInitiative()
          } else {
            val candidates = list.collect {
              case QueryResult(dot, OscInjectReply.Accepted) => dot
            }
            if (candidates.isEmpty) { // there is no need to 'complete' the transaction in that case
              log("No node accepted the injection")
              retryInitiative()
            } else {
              val candidate = Util.choose(candidates)
              c.sendVideos(OscInjectCommit(Uid, targetDot = candidate))
              testEjectAndGoBackToIdle()
            }
          }

        case Failure(ex) =>
          log(s"Failed to ping - ${ex.getClass.getSimpleName}.")
          retryInitiative()
      }}
    }
  }

  private def retryInitiative()(implicit tx: InTxn): Unit = {
    val oldState = stateRef.swap(Idle)
    assert(oldState == InjectQuery || oldState == InjectPending)
    log("Retrying initiative in 4 seconds.")
    scheduleInitiative(4f)
  }

  def init()(implicit tx: InTxn): Unit = {
    if (!config.debugText) Swing.onEDT(view.start())

    becomeIdle()
  }

  private def testEjectAndGoBackToIdle()(implicit tx: InTxn): Unit = {
    log("Test ejection")
    stateRef()  = Ejecting
    val durSecs = 10f
    val delay   = (durSecs * 1000).toLong
    /* val task = */ c.scheduleTxn(delay)(tx => becomeIdle()(tx))
  }

  private def testInjectAndGoBackToIdle()(implicit tx: InTxn): Unit = {
    log("Test injection")
    stateRef()  = Injecting
    val durSecs = 10f
    val delay   = (durSecs * 1000).toLong
    /* val task = */ c.scheduleTxn(delay)(tx => becomeIdle()(tx))
  }

  private def becomeIdle()(implicit tx: InTxn): Unit = {
    log("Becoming idle.")
    val now       = System.currentTimeMillis()
    val idleDur   = Util.rrand(config.textMinDur, config.textMaxDur)
    val minTime   = now + (config.textMinDur * 1000).toLong
    idleMinTime() = minTime
    stateRef()    = Idle
    scheduleInitiative(idleDur)
  }

  private def scheduleInitiative(durSecs: Float)(implicit tx: InTxn): Unit = {
    val delay = (durSecs * 1000).toLong
    val task  = c.scheduleTxn(delay)(tx => startInitiative()(tx))
    idleTask.swap(Some(task)).foreach(_.cancel())
  }
}