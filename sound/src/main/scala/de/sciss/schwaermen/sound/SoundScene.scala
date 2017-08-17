/*
 *  SoundScene.scala
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
package sound

import de.sciss.file._
import de.sciss.lucre.synth.{Buffer, InMemory, Server, Synth, Txn}
import de.sciss.schwaermen.sound.Main.log
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.{Curve, SynthGraph, freeSelf}

import scala.Predef.{any2stringadd => _, _}

final class SoundScene(config: Config, relay: RelayPins) {
  type S = InMemory

  private[this] val system  = InMemory()
  private[this] val aural   = AuralSystem()

  def run(): this.type = {
    system.step { implicit tx =>
      aural.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = {
          booted(aural, s)(system.wrap(tx.peer))
        }

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
      aural.start()
    }
    this
  }

  private[this] lazy val testGraph: SynthGraph = SynthGraph {
    import de.sciss.synth.Ops.stringToControl
    import de.sciss.synth.ugen._
    val freq  = LFNoise0.ar(10).linexp(-1, 1, 200, 6000)
    val osc   = SinOsc.ar(freq) * 0.33
    val line  = Line.ar(1, 0, 2, doneAction = freeSelf)
    val sig   = osc * line
    Out.ar("bus".kr, sig)
  }

  private[this] val diskGraph: SynthGraph = SynthGraph {
    import de.sciss.numbers.Implicits._
    import de.sciss.synth.Ops.stringToControl
    import de.sciss.synth.ugen._
    val bus     = "bus"     .ir
    val buf     = "buf"     .ir
    val dur     = "dur"     .ir
    val fdIn    = "fadeIn"  .ir
    val fdOut   = "fadeOut" .ir
    val disk    = VDiskIn.ar(numChannels = 2, buf = buf, speed = BufRateScale.ir(buf), loop = 0)
    val chan    = Select.ar(bus, disk)
    val hpf     = HPF.ar(chan, 80f)
    val env     = Env.linen(attack = fdIn, sustain = dur - (fdIn + fdOut), release = fdOut, curve = Curve.sine)
    val gain    = 9.dbamp
    val eg      = EnvGen.ar(env, levelScale = gain /* , doneAction = freeSelf */)
    val done    = Done.kr(eg)
    val limDur  = 0.01f
    val limIn   = hpf * eg
    val lim     = Limiter.ar(limIn * gain, level = -0.2.dbamp, dur = limDur)
    FreeSelf.kr(TDelay.kr(done, limDur * 2))
    val sig     = lim
    Out.ar(bus, sig)
  }

  def testPing(ch: Int): Unit = {
    system.step { implicit tx =>
      aural.serverOption.foreach { s =>
        Synth.play(testGraph, nameHint = Some("test"))(s, args = "bus" -> ch :: Nil)
      }
    }
  }

  private[this] val soundDir  = config.baseDir / "sound"
  private[this] val pathFmt   = "gertrude_text%d.aif"

//  private[this] val synthRef  = Ref(Option.empty[Synth])

  def play(textId: Int, ch: Int, start: Long, stop: Long, fadeIn: Float, fadeOut: Float): Unit = {
//    system.step { implicit tx =>
//      synthRef.swap(None).foreach(_.dispose())
//    }
    if (!config.isLaptop) {
      relay.selectChannel(ch)
    }
    val bus = ch / 6
    system.step { implicit tx =>
      aural.serverOption.foreach { s =>
        val target  = s.defaultGroup
        target.freeAll()
        val path    = (soundDir / pathFmt.format(textId + 1)).path
        val buf     = Buffer.diskIn(s)(path = path, startFrame = start, numChannels = 2)
        val dur     = math.max(0L, stop - start) / Vertex.SampleRate
        val syn     = Synth.play(diskGraph, nameHint = Some("disk"))(target = target,
          args = List("bus" -> bus, "buf" -> buf.id, "dur" -> dur, "fadeIn" -> fadeIn, "fadeOut" -> fadeOut),
          dependencies = buf :: Nil)
        syn.onEndTxn { implicit tx =>
          buf.dispose()
//          if (synthRef().contains(syn)) synthRef() = None
        }
//        synthRef() = Some(syn)
      }
    }
  }

  def booted(aural: AuralSystem, s: Server)
            (implicit tx: S#Tx): Unit = {
    tx.afterCommit(log("scsynth booted"))
  }
}