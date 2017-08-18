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

import java.io.{BufferedInputStream, DataInputStream, InputStream}
import java.nio.ByteBuffer

import de.sciss.file._
import de.sciss.lucre.synth.{Buffer, InMemory, Server, Synth, Txn}
import de.sciss.schwaermen.sound.Main.log
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.{Curve, SynthDef, SynthGraph, UGenGraph, addToHead, freeSelf}

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

  private[this] lazy val pingGraph: SynthGraph = SynthGraph {
    import de.sciss.synth.Ops.stringToControl
    import de.sciss.synth.ugen._
    val freq  = LFNoise0.ar(10).linexp(-1, 1, 200, 6000)
    val osc   = SinOsc.ar(freq) * 0.33
    val line  = Line.ar(1, 0, 2, doneAction = freeSelf)
    val sig   = osc * line
    Out.ar("bus".kr, sig)
  }

  private[this] lazy val noisePulseGraph: SynthGraph = SynthGraph {
    import de.sciss.synth.Ops.stringToControl
    import de.sciss.synth.ugen._
    val noise = PinkNoise.ar(0.5)
    val hpf   = HPF.ar(noise, 80)
    val sig   = hpf * LFPulse.ar(2)
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

  private def playTestGraph(s: Server, graph: SynthGraph, ch: Int)(implicit tx: S#Tx): Boolean = {
    Synth.play(graph, nameHint = Some("test"))(target = s.defaultGroup, args = "bus" -> ch :: Nil)
    true
  }

  // bloody JDK doesn't have shit
  // cf. https://stackoverflow.com/questions/4332264/wrapping-a-bytebuffer-with-an-inputstream
  private final class ByteBufferInputStream(buf: ByteBuffer) extends InputStream {
    def read: Int =
      if (!buf.hasRemaining) -1
      else buf.get & 0xFF

    override def read(bytes: Array[Byte], off: Int, len0: Int): Int =
      if (!buf.hasRemaining) -1
      else {
        val len = math.min(len0, buf.remaining)
        buf.get(bytes, off, len)
        len
      }
  }

  @inline private[this] def readPascalString(dis: DataInputStream): String = {
    val len   = dis.readUnsignedByte()
    val arr   = new Array[Byte](len)
    dis.read(arr)
    new String(arr)
  }

  private def readSynthDef(b: ByteBuffer): List[SynthDef] = {
    val COOKIE  = 0x53436766  // 'SCgf'
    val is  = new ByteBufferInputStream(b)
    val dis = new DataInputStream(new BufferedInputStream(is))
    try {
      val cookie  = dis.readInt()
      require (cookie == COOKIE, s"Buffer must begin with cookie word 0x${COOKIE.toHexString}")
      val version = dis.readInt()
      require (version == 1 || version == 2, s"Buffer has unsupported version $version, required 1 or 2")
      val numDefs = dis.readShort()
      List.fill(numDefs) {
        val name  = readPascalString(dis)
        val graph = UGenGraph.read(dis, version = version)
        new SynthDef(name, graph)
      }

    } finally {
      dis.close()
    }
  }

  def testSound(ch: Int, tpe: Int, rest: Seq[Any]): Boolean = {
    system.step { implicit tx =>
      aural.serverOption.fold(true) { s =>
        val target  = s.defaultGroup
        target.freeAll()
        tpe match {
          case 0 => playTestGraph(s, pingGraph      , ch = ch)
          case 1 => playTestGraph(s, noisePulseGraph, ch = ch)
          case 2 => rest match {
            case Seq(b: ByteBuffer) =>
              readSynthDef(b).headOption.fold(false) { df =>
                val syn = Synth.expanded(s, df.graph)
                syn.play(target = target, args = "bus" -> ch :: Nil, addAction = addToHead, dependencies = Nil)
                true
              }

            case other =>
              Console.err.println(s" test sound tpe $other")
              false
          }
          case other =>
            Console.err.println(s"Unsupported test sound tpe $other")
            false
        }
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
        // avoid clicking
        val fdIn1   = if (fadeIn  > 0) fadeIn  else 0.01f
        val fdOut1  = if (fadeOut > 0) fadeOut else 0.01f
        val syn     = Synth.play(diskGraph, nameHint = Some("disk"))(target = target,
          args = List("bus" -> bus, "buf" -> buf.id, "dur" -> dur, "fadeIn" -> fdIn1, "fadeOut" -> fdOut1),
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