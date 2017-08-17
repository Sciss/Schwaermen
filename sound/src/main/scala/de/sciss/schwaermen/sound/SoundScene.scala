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
import de.sciss.synth.{SynthGraph, freeSelf}

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
    ???
//    import de.sciss.synth.Ops.stringToControl
//    import de.sciss.synth.ugen._
//    val freq  = LFNoise0.ar(10).linexp(-1, 1, 200, 6000)
//    val osc   = SinOsc.ar(freq) * 0.33
//    val line  = Line.ar(1, 0, 2, doneAction = freeSelf)
//    val sig   = osc * line
//    Out.ar("bus".kr, sig)
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

  def play(textId: Int, ch: Int, start: Long, stop: Long, fadeIn: Float, fadeOut: Float): Unit = {
    if (!config.isLaptop) {
      relay.selectChannel(ch)
    }
    val bus = ch / 6
    system.step { implicit tx =>
      aural.serverOption.foreach { s =>
        val path  = (soundDir / pathFmt.format(textId + 1)).path
        val buf   = Buffer.diskIn(s)(path = path, startFrame = start, numChannels = 2)
        val dur   = ??? : Float
        val syn   = Synth.play(diskGraph, nameHint = Some("disk"))(target = s.defaultGroup,
          args = List("bus" -> bus, "buf" -> buf.id, "dur" -> dur), dependencies = buf :: Nil)
        syn.onEndTxn { implicit tx => buf.dispose() }
      }
    }
  }

  def booted(aural: AuralSystem, s: Server)
            (implicit tx: S#Tx): Unit = {
    log("scsynth booted")
  }
}