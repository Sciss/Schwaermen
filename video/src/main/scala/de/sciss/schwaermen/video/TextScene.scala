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

import de.sciss.numbers.Implicits._
import de.sciss.osc
import de.sciss.schwaermen.video.Scene.OscReplyInjection
import de.sciss.schwaermen.video.TextScene.{Ejecting, Idle, Pending, Querying, State}

import scala.concurrent.stm.{InTxn, Ref}
import scala.swing.Swing
import scala.util.{Failure, Random, Success}

object TextScene {
  private sealed trait State
  private case object Idle      extends State
  private case object Querying  extends State
  private case object Pending   extends State
  private case object Ejecting  extends State
}
final class TextScene(c: OSCClient, r: Random) extends Scene.Text {
  private[this] val config = c.config

  private[this] val stateRef = Ref[State](Idle)

  private[this] val videoIdx: Int = {
    val res = Network.videoDotSeq.indexOf(c.dot)
    if (res >= 0) res else {
      Console.err.println(s"WARNING: No dedicated video text for ${c.dot}. Using first instead")
      0
    }
  }

  private[this] val text: String = {
    Util.readTextResource(s"text${videoIdx + 1}.txt").replaceAll("\n", " ——— ") // XXX TODO decide here
  }

  private[this] val gl = Glyphosat(config, text, r)

//  private[this] lazy val debugView: DebugTextView = new DebugTextView
  private[this] lazy val view     : TextView      = new TextView(config, gl)

  /* Pixels per second */
  private[this] val textSpeed         : Float = config.textVX * config.fps
  private[this] val pixelsUntilTimeOut: Float = textSpeed * Network.TimeOutSeconds

  private[this] val idleTask = Ref(Option.empty[Task])

  if (!config.debugText) Swing.onEDT {
    view
  }

  def queryInjection(sender: SocketAddress)(implicit tx: InTxn): Unit = {
    val accepted = if (stateRef() == Idle) {
      stateRef() = Pending
      true
    } else {
      false
    }
    val reply = OscReplyInjection(accepted = accepted)
    c.queryVideos(reply) {
      case _ => ???
    } { implicit tx => {
      case Success(_) =>
      case Failure(_) =>
    }}
  }

  private def startInitiative()(implicit tx: InTxn): Unit = {
    idleTask() = None
    if (stateRef() == Idle) {
      stateRef() = Querying
      debugPrint("Starting initiative")
      c.queryVideos(osc.Message("/ping")) {
        case osc.Message("/pong", i: Int) => i
      } { implicit tx => {
        case Success(Nil) =>
          debugPrint("Success, but no known nodes visible.")
          retryInitiative()
        case Success(list) =>
          debugPrint(s"Success, got $list values")
          stateRef() = Ejecting
        case Failure(ex) =>
          debugPrint(s"Failed to ping - ${ex.getClass.getSimpleName}.")
          retryInitiative()
      }}
    }
  }

  private def retryInitiative()(implicit tx: InTxn): Unit = {
    assert(stateRef.swap(Idle) == Querying)
    debugPrint("Retrying initiative in 4 seconds.")
    scheduleInitiative(4f)
  }

  private def debugPrint(what: String): Unit = if (config.debugText) println(s"DEBUG: $what")

  def init()(implicit tx: InTxn): Unit = {
    if (!config.debugText) Swing.onEDT(view.start())

    val idleDur = r.nextFloat().linlin(0f, 1f, config.textMinDur, config.textMaxDur)
    scheduleInitiative(idleDur)
  }

  private def scheduleInitiative(durSecs: Float)(implicit tx: InTxn): Unit = {
    val delay = (durSecs * 1000).toLong
    val task  = c.scheduleTxn(delay)(tx => startInitiative()(tx))
    idleTask.swap(Some(task)).foreach(_.cancel())
  }
}