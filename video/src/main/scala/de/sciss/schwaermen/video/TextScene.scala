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

import scala.concurrent.stm.{InTxn, Ref, Txn, atomic}
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

  private[this] val config      = c.config
  private[this] val videoId     = c.videoId

  private[this] val stateRef    = Ref[State](Idle)
  private[this] val idleMinTime = Ref(0L)

  private[this] val gl = Glyphosat(config, c.vertices)

  private[this] lazy val view: TextView = new TextView(config, gl, videoId = videoId)

  private[this] val idleTask = Ref(Option.empty[Task])

  if (!config.debugText) Swing.onEDT {
    view
  }

  def queryInjection(sender: SocketAddress, Uid: Long, meta: TextPathFinder.Meta,
                     ejectVideoId: Int, ejectVertex: Int)(implicit tx: InTxn): Unit = {
    if (stateRef() == Idle) {
      val sourceVertex    = (ejectVertex + meta.vertexOffset(ejectVideoId)).toShort
      val pathLen         = ??? : Int
      val anticipatedDur  = pathLen * Glyphosat.AvgVertexDur
      val targetVertex    = (rnd.nextInt(meta.textLen(videoId)) + meta.vertexOffset(videoId)).toShort

      val t1    = System.currentTimeMillis()
      val path  = meta.finder.findExtendedPath(sourceVertex = sourceVertex, targetVertex = targetVertex, pathLen = pathLen)
      val t2    = System.currentTimeMillis()
      if (config.isLaptop) {
        val slow = (t2 - t1) * 10
        Thread.sleep(slow)    // cheesy way to come closer to the Raspi experience
      }
      assert(path.length == pathLen)
      val vertices  = path.map(meta.vertex(_))
      val pathDur   = vertices.iterator.map(_.netDuration).sum
      log(f"path length $pathLen; dur $pathDur%1.1f vs. pathLen * avgDur = $anticipatedDur")

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
      val Uid             = c.mkTxnId()
      val expectedDelay   = config.queryPathDelay
      val expectedDelayMS = (expectedDelay * 1000).toLong
      // add three seconds so the word can bubble upwards in a diagonal way
      val ejectVertex = gl.ejectionCandidate(delay = expectedDelay + 3f)
      log(s"EjectionCandidate is $ejectVertex ${gl.vertices(ejectVertex).quote}")
      c.queryVideos(OscInjectQuery(uid = Uid, videoId = videoId, vertex = ejectVertex),
        extraDelay = expectedDelayMS) {
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
              performEjection(ejectVertex = ejectVertex)
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
    val dur = Util.rrand(3.5f, 5.0f)
    log(f"Retrying initiative in $dur%1.1f seconds.")
    scheduleInitiative(dur)
  }

  def init()(implicit tx: InTxn): Unit = {
    if (!config.debugText) Swing.onEDT(view.start())

    becomeIdle()
  }

  private def performEjection(ejectVertex: Int)(implicit tx: InTxn): Unit = {
    log("performEjection")
    stateRef()      = Ejecting
    val timeOutSec  = 30f
    val timeOutDly  = (timeOutSec * 1000).toLong
    val timeOut     = c.scheduleTxn(timeOutDly) { tx =>
      log("ejection timeout reached")
      becomeIdle()(tx)
    }
    Txn.afterCommit(_ => gl.eject(ejectVertex)(ejected(timeOut)))
  }

  private def ejected(timeOut: Task): Unit = {
    log("Gl notifies ejection done")
    atomic { implicit tx =>
      if (!timeOut.wasExecuted) {
        timeOut.cancel()
        becomeIdle()  // XXX TODO --- notify injection about beginning sound gesture
      }
    }
  }

  private def testInjectAndGoBackToIdle()(implicit tx: InTxn): Unit = {
    log("Test injection")
    stateRef()  = Injecting
    val durSecs = 10f
    val delay   = (durSecs * 1000).toLong
    /* val task = */ c.scheduleTxn(delay)(tx => becomeIdle()(tx))
  }

  private def becomeIdle()(implicit tx: InTxn): Unit = {
    val now       = System.currentTimeMillis()
    val idleDur   = Util.rrand(config.textMinDur, config.textMaxDur)
    log(f"Becoming idle for $idleDur%1.1f seconds.")
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