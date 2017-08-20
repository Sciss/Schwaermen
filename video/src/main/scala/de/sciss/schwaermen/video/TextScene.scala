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
import de.sciss.schwaermen.video.Glyphosat.EjectionCandidate
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

  // XXX TODO --- this could go into OSCClient, so we don't need two
  // instances, one per Scene; this requires that access is single threaded,
  // i.e. guaranteed to come only from the OSC thread
  private[this] val speakerFinder = new SpeakerPathFinder(c.speakers)

  if (!config.debugText) Swing.onEDT {
    view
  }

  def queryInjection(sender: SocketAddress, Uid: Long, meta: TextPathFinder.Meta,
                     ejectVideoId: Int, ejectVertex: Int)(implicit tx: InTxn): Unit = {
    if (stateRef() == Idle) {
      log(s"queryInjection; ejectVideoId $ejectVideoId, $ejectVertex")
      val spkSrcVertex    = c.speakers.exits(ejectVideoId).toShort
      val spkTgtVertex    = c.speakers.exits(videoId     ).toShort
      val spkPath         = speakerFinder.findPath(spkSrcVertex, spkTgtVertex)
      val pathLen         = spkPath.length
      val anticipatedDur  = pathLen * Glyphosat.AvgVertexDur
      val txtSrcVertex    = (ejectVertex + meta.vertexOffset(ejectVideoId)).toShort
      // XXX TODO:
      val txtTgtVertex    = (rnd.nextInt(meta.textLen(videoId)) + meta.vertexOffset(videoId)).toShort

      val t1          = System.currentTimeMillis()
      val textIdxPath = meta.finder
        .findExtendedPath(sourceVertex = txtSrcVertex, targetVertex = txtTgtVertex, pathLen = pathLen)
      val t2        = System.currentTimeMillis()
      if (config.isLaptop) {
        val slow = (t2 - t1) * 10
        Thread.sleep(slow)    // cheesy way to come closer to the Raspi experience
      }
      assert(textIdxPath.length == pathLen)
      val textPath  = textIdxPath.map(meta.vertex(_))
      val pathDur   = textPath.iterator.map(_.netDuration).sum
      log(f"path length $pathLen; dur $pathDur%1.1f vs. pathLen * avgDur = $anticipatedDur")

      stateRef() = InjectPending
      val reply = OscInjectReply(Uid, OscInjectReply.Accepted)
      c.queryTxn(sender, reply) {
        case OscInjectCommit(Uid, targetDot)  => targetDot === c.dot
        case OscInjectAbort (Uid)             => false
      } { implicit tx => {
        case Success(QueryResult(_ /* dot */, true)) =>
          performInjection(spkPath, textPath)
        case other =>
          log(s"Injection target received abortion or was not selected - $other")
          retryInitiative()
      }}
    } else {
      val reply = OscInjectReply(Uid, OscInjectReply.Rejected)
      c.sendTxn(sender, reply)
    }
  }

  /*

    (3) attempts to start an ejection initiative.
    Goes into mode 'inject-query':
    - chooses an ejection candidate
    - queries the network for injection candidates.
      if aborted or none found, retries the initiative shortly after


   */
  private def startInitiative()(implicit tx: InTxn): Unit = {
    idleTask() = None
    if (stateRef() != Idle) return

    stateRef() = InjectQuery
    log("Starting initiative")
    val Uid             = c.mkTxnId()
    val expectedDelay   = config.queryPathDelay
    val expectedDelayMS = (expectedDelay * 1000).toLong
    // add three seconds so the word can bubble upwards in a diagonal way
    val ejectCandidate = gl.ejectionCandidate(delay = expectedDelay + 3f)
    val ejectVertex = ejectCandidate.vertexIdx
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
            log(s"Commit ejection - candidate $candidate")
            c.sendVideos(OscInjectCommit(Uid, targetDot = candidate))
            performEjection(ejectCandidate)
          }
        }

      case Failure(ex) =>
        log(s"Failed to ping - ${ex.getClass.getSimpleName}.")
        retryInitiative()
    }}
  }

  private def retryInitiative()(implicit tx: InTxn): Unit = {
    val oldState = stateRef.swap(Idle)
    assert(oldState == InjectQuery || oldState == InjectPending)
    val dur = Util.rrand(3.5f, 5.0f)
    log(f"Retrying initiative in $dur%1.1f seconds.")
    scheduleInitiative(dur)
  }

  /*

   (1) Moves into state 'idle'.

   */
  def init()(implicit tx: InTxn): Unit = {
    if (!config.debugText) Swing.onEDT(view.start())

    becomeIdle()
  }

  private final class Ejector(timeOut: Task)
    extends Glyphosat.Ejector {

    /** Called when the ejecting word moves above vertical threshold. */
    def ejectWordThresh(): Unit = {
      log("ejectWordThresh")
      log("TODO --- ONLY HERE SHOULD SOUND BEGIN")
//      atomic { implicit tx =>
//        if (!timeOut.wasExecuted) {
//          timeOut.cancel()
//        }
//      }
    }

    /** Called when all words have moved out after ejection. */
    def ejectAllClear(): Unit = {
      log("ejectAllClear")
      atomic { implicit tx =>
        if (!timeOut.wasExecuted) {
          timeOut.cancel()
          log("TODO --- GO TO TRUNK STATE")
          Txn.afterCommit(_ => gl.reset())
          becomeIdle()  // XXX TODO --- notify injection about beginning sound gesture
        }
      }
    }
  }

  private def performEjection(ec: EjectionCandidate)(implicit tx: InTxn): Unit = {
    log("performEjection")
    stateRef()      = Ejecting
    val timeOutSec  = ec.expectedDelay + 60f
    val timeOutDly  = (timeOutSec * 1000).toLong
    val timeOut     = c.scheduleTxn(timeOutDly) { tx =>
      log("performEjection - timeout reached")
      becomeIdle()(tx)
    }
    Txn.afterCommit(_ => gl.eject(ec, new Ejector(timeOut)))
  }

  private[this] val injSpkPath    = Ref(Array.empty[Spk   ])
  private[this] val injTextPath   = Ref(Array.empty[Vertex])
  private[this] val injSpkPathIdx = Ref(0)

  private def injectStep()(implicit tx: InTxn): Unit = {
    val spkPath   = injSpkPath()
    val textPath  = injTextPath()
    val idx       = injSpkPathIdx.getAndTransform(_ + 1)
    log(s"injectStep $idx")
    val isLast    = idx + 1 >= spkPath.length
    if (idx < spkPath.length) {  // this guard is needed for the case where the path is empty
      val spk = spkPath(idx)
      val txt = textPath(idx)
      val fadeIn0   = txt.fadeInSec
      val fadeIn    =
        if (fadeIn0  == 0 || idx == 0 || spkPath(idx - 1).canOverlap(spk)) fadeIn0
        else math.min(fadeIn0, 0.1f)
      val fadeOut0  = txt.fadeOutSec
      val fadeOut   =
        if (fadeOut0 == 0 || isLast   || spkPath(idx + 1).canOverlap(spk)) fadeOut0
        else math.min(fadeIn0, 0.1f)
      val start     = txt.span.start + ((fadeIn0  - fadeIn ) * Vertex.SampleRate).toLong
      val stop      = txt.span.stop  - ((fadeOut0 - fadeOut) * Vertex.SampleRate).toLong
      c.soundNode(spk.dot).foreach { target =>
        c.sendTxn(target, Network.OscPlayText(
          textId = txt.textId, ch = spk.ch, start = start, stop = stop, fadeIn = fadeIn, fadeOut = fadeOut))
      }
      if (!isLast) {
        val delay = ((stop - start) / Vertex.SampleRate * 1000).toLong
        c.scheduleTxn(delay)(tx => injectStep()(tx))
      }
    }
    if (isLast) {
      becomeIdle()
    }
  }

  private def performInjection(spkPath: Array[Spk], textPath: Array[Vertex])(implicit tx: InTxn): Unit = {
    log("performInjection")
    stateRef()      = Injecting
    injSpkPath()    = spkPath
    injTextPath()   = textPath
    injSpkPathIdx() = 0
    injectStep()
  }

  /*

    (2) let's the text flow for a random period between `textMinDur` and `textMaxDur`.
    then starts an initiative.

   */
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