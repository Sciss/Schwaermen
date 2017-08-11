/*
 *  TestPaths.scala
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

package de.sciss.schwaermen.sound

import de.sciss.lucre.synth.{InMemory, Server, Synth, Txn}
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.{SynthGraph, freeSelf}

final class TestPaths {
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

  private[this] val playGraph: SynthGraph = SynthGraph {
    import de.sciss.synth.Ops.stringToControl
    import de.sciss.synth.ugen._
    val freq  = LFNoise0.ar(10).linexp(-1, 1, 200, 6000)
    val osc   = SinOsc.ar(freq) * 0.33
    val line  = Line.ar(1, 0, 2, doneAction = freeSelf)
    val sig   = osc * line
    Out.ar("bus".kr, sig)
  }

  def ping(ch: Int): Unit = {
    system.step { implicit tx =>
      aural.serverOption.foreach { s =>
        Synth.play(playGraph, nameHint = Some("test"))(s, args = "bus" -> ch :: Nil)
      }
    }
  }

  def booted(aural: AuralSystem, s: Server)
            (implicit tx: S#Tx): Unit = {
  }
}