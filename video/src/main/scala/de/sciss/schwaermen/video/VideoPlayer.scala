package de.sciss.schwaermen
package video

import de.sciss.file._
import de.sciss.schwaermen.video.Main.log

import scala.sys.process.ProcessLogger

final class VideoPlayer(config: Config) {
  private sealed trait Status
  private case object Idle    extends Status
  private case object Playing extends Status

  @volatile
  private[this] var status: Status = Idle

  @volatile
  private[this] var playIssued: TrunkPlay = _

  @volatile
  private[this] var playActive: TrunkPlay = _

  private[this] val playSync = new AnyRef

  @volatile
  private[this] var winX: Int = config.omxWinX

  @volatile
  private[this] var winY: Int = config.omxWinY

  private[this] val script = config.baseDir/"dbuscontrol.sh"
//  private[this] val script = config.baseDir/"imperfect_dbus.sh"
  require(script.canExecute, s"${script.name}  - script is not executable!")

//  private def sendStatus(): Unit = client ! osc.Message("/status", status.id)

  def issuePlay(play: TrunkPlay): Unit =
    playSync.synchronized {
      playIssued = play
      status = Playing
      playSync.notify()
    }

  private def secsToHHMMSS(secs: Float): String = {
    val sec  = secs.toInt
    val secM = sec % 60
    val min  = sec / 60
    val minM = min % 60
    val hour = min / 60
    f"$hour%02d:$minM%02d:$secM%02d"
  }

  private[this] val logNone     = ProcessLogger((_: String) => ())
  private[this] val logErrors   = ProcessLogger((_: String) => (), (s: String) => Console.err.println(s))
  private[this] val hasDbusName = !config.dbusName.isEmpty
  private[this] val dbusIsInc   = config.dbusName.contains("%d")
  private[this] var dbusCount   = 0

  private def mkDbusName(inc: Boolean): String = {
    if (dbusIsInc) {
      if (inc) dbusCount += 1
      config.dbusName.format(dbusCount)
    } else if (hasDbusName) {
      config.dbusName
    } else {
      "org.mpris.MediaPlayer2.omxplayer"
    }
  }

  private[this] lazy val conFadeThread: Thread = new Thread {
    override def run(): Unit = {
      // ---- second stage: play videos ----
      while (true) {
        val pl = playSync.synchronized {
          if (playIssued == null) {
            playSync.wait()
          }
          val res = playIssued
          playActive  = res
          playIssued  = null
          res
        }

        log(s"player - $pl")
        if (pl.delay > 0) {
          Thread.sleep((pl.delay * 1000).toLong)
        }

        val videoF = pl.file(config) //  config.baseDir/"videos"/pl.file
        if (!videoF.isFile) {
          println(s"WARNING: video file '$videoF' does not exist.")
        }

        // Play(file: String, start: Float, duration: Float, orientation: Int, fadeIn: Float, fadeOut: Float)
        val cmdB = List.newBuilder[String]
//        cmdB += "sudo"
        cmdB += "omxplayer"
        if (hasDbusName) {
          cmdB += "--dbus_name"
          cmdB += mkDbusName(inc = true)
        }
        cmdB += "--no-osd"
//        cmdB += "-b"
        if (pl.orientation != 0) {
          cmdB += "--orientation"
          cmdB += pl.orientation.toString
        }
        if (pl.start > 0) {
          cmdB += "--pos"
          cmdB += secsToHHMMSS(pl.start)
        }
        if (pl.fadeIn > 0) {
          cmdB += "--alpha"
          cmdB += "0"
        }
        if (config.smallWindow) {
          cmdB += "--win"
          cmdB += "384,256,896,768"
        } else if (winX >= 0 && winY >= 0) {
          cmdB += "--win"
          cmdB += s"$winX,$winY,${winX + 1024},${winY + 1024}"
        }
        cmdB += videoF.path

        val cmd = cmdB.result()
        log(cmd.mkString(" "))
        import sys.process._
        val omx = cmd.run(logErrors)
        val t1  = System.currentTimeMillis()

        // it takes a moment till the dbus client is registered.
        // if we run the script with the 'status' command, it will
        // have an error code of 1 while the client is not yet
        // visible, and zero when it's there.
        var launched = false
        while (!launched && (System.currentTimeMillis() - t1 < 2000)) {
          val res = runScript("status" :: Nil, ignoreError = true)
          launched = res == 0
          if (!launched) Thread.sleep(0)
        }
        log(s"player - omx launched")

        if (pl.fadeIn > 0) {
          fadeIn(pl.fadeIn)
        }
        val t2      = System.currentTimeMillis()
        val secFdIn = (t2 - t1) / 1000.0
        val secRem  = pl.duration - secFdIn - pl.fadeOut
        val milRem  = math.max(0L, (secRem * 1000).toLong)
        Thread.sleep(milRem)
        if (pl.fadeOut > 0) {
          fadeOut(pl.fadeOut)
        }
        log(s"player - stopping omx")
        stopVideo()
        omx.exitValue() // wait for process to terminate
        log(s"player - omx stopped")
        Thread.sleep(1000)  // XXX TODO --- does this help with dbus registry?

        // signalise to control
        status = Idle
//        sendStatus()
      }
    }
  }

  def start(): Unit =
    conFadeThread.start()

  private def setAlpha(i: Int): Int = {
    val alpha = math.max(0, math.min(255, i))
    runScript("setalpha" :: alpha.toString :: Nil)
  }

  private def fade(from: Float, to: Float, dur: Float): Unit = {
    val start = (from * 255 + 0.5f).toInt
    val end   = (to   * 255 + 0.5f).toInt
    val durM  = (dur * 1000).toInt
    val t1    = System.currentTimeMillis
    // val t3    = t1 + durM
    //     println(s"t1 = $t1, t3 = $t3, start = $start, end - $end")
    setAlpha(start)
    var last  = start
    while (last != end) {
      val t2   = System.currentTimeMillis
      val frac = math.min(1.0, (t2.toDouble - t1) / durM)
      val curr = ((frac * (to - from) + from) * 255 + 0.5).toInt
      //        val curr = (t2.linlin(t1, t3, from, to) + 0.5).toInt
      //        println(s"t2 = $t2, curr = $curr")
      if (curr != last) {
        setAlpha(curr)
        last = curr
      } else {
        // Thread.`yield()`
        Thread.sleep(0)
      }
    }
    // println("Done.")
  }

  private def fadeIn (dur: Float): Unit = fade(from = 0f, to = 1f, dur = dur)
  private def fadeOut(dur: Float): Unit = fade(from = 1f, to = 0f, dur = dur)

  private def runScript(args: List[String], ignoreError: Boolean = false): Int = {
    import sys.process._
    val pb = script.path :: args
    val res = if (ignoreError) pb.!<(logNone) else pb.!
    res
  }

  private def stopVideo(): Unit = runScript("stop" :: Nil)
}
