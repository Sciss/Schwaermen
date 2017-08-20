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
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.{Buffer, InMemory, Server, Synth, Txn}
import de.sciss.schwaermen.sound.Main.log
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.{Curve, SynthDef, SynthGraph, UGenGraph, addAfter, addToHead, freeSelf}

import scala.Predef.{any2stringadd => _, _}
import scala.concurrent.stm.Ref
import scala.util.Random
import scala.util.control.NonFatal

final class SoundScene(c: OSCClient) {
  type S = InMemory

  import c.relay

  private[this] val config = c.config

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
    val gain    = 12.dbamp
    val eg      = EnvGen.ar(env, levelScale = gain /* , doneAction = freeSelf */)
    val done    = Done.kr(eg)
//    val limDur  = 0.01f
    val limIn   = hpf * eg
//    val lim     = Limiter.ar(limIn /* * gain */, level = -0.2.dbamp, dur = limDur)
    FreeSelf.kr(done) // TDelay.kr(done, limDur * 2))
    val sig     = limIn // lim
    Out.ar(bus, sig)
  }

  private[this] val masterGraph: SynthGraph = SynthGraph {
    import de.sciss.numbers.Implicits._
    import de.sciss.synth.Ops.stringToControl
    import de.sciss.synth.ugen._
    val in      = In.ar(0, 2)
    val amp     = "amp".kr(1f)
    val limDur  = 0.01f
    val limIn   = in * amp
    val lim     = Limiter.ar(limIn, level = -0.2.dbamp, dur = limDur)
    val sig     = lim
    ReplaceOut.ar(0 /* bus */, sig)
  }

  private[this] val masterSynth = Ref(Option.empty[Synth])

  def setMasterVolume(amp: Float): Unit = {
    system.step { implicit tx =>
      masterSynth().foreach(_.set("amp" -> amp))
    }
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
  private[this] val beeDir    = soundDir / "loops"
  private[this] val beeFmt    = "%d_beesLoop.aif"

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

  // ---- BEES  ----

  final val BEE_COOKIE = 0x42656573 // "Bees"

  private[this] lazy val oneBee = Bee(id = 7292, numChannels = 2, numFrames = 981797, amp = -23.0f)

  private def readBees(): Array[Bee] = {
    val is = getClass.getResourceAsStream("/bees.bin")
    if (is == null) {
      Console.err.println("Cannot read 'bees.bin' resource!")
      Array(oneBee)
  
    } else {
      try {
        val dis = new DataInputStream(is)
        val cookie = dis.readInt()
        require (cookie == BEE_COOKIE, s"Unexpected cookie ${cookie.toHexString} -- expected ${BEE_COOKIE.toHexString}")
        val num = dis.readShort()
        Array.fill(num) {
          val id          = dis.readInt  ()
          val numChannels = dis.readShort()
          val numFrames   = dis.readInt  ()
          val gain        = dis.readFloat()
          import de.sciss.numbers.Implicits._
          val amp         = math.min(1f, gain.dbamp)
          Bee(id = id, numChannels = numChannels, numFrames = numFrames, amp = amp)
        }
      } catch {
        case NonFatal(ex) =>
          Console.err.println("Error reading 'bees.bin' resource:")
          ex.printStackTrace()
          Array(oneBee)

      } finally {
        is.close()
      }
    }
  }

  private final class Bee(val id: Int, val numChannels: Int, val numFrames: Int, val amp: Float)

  private def Bee(id: Int, numChannels: Int, numFrames: Int, amp: Float) =
    new Bee(id = id, numChannels = numChannels, numFrames = numFrames, amp = amp)

  private def mkBeeGraph(numCh: Int): SynthGraph = SynthGraph {
    import de.sciss.synth._
    import Ops.stringToControl
    import ugen._
    val bus     = "bus"     .ir
    val buf     = "buf"     .ir
    val dur     = "dur"     .ir
    val fdIn    = "fadeIn"  .ir
    val fdOut   = "fadeOut" .ir
    val disk    = VDiskIn.ar(numChannels = numCh, buf = buf, speed = BufRateScale.ir(buf), loop = 1)
    disk.poll(1, "disk")
    val chan    = if (numCh == 1) disk else {
      val pan   = LFNoise1.kr(1.0 / 45)
      LinXFade2.ar(disk \ 0, disk \ 1, pan)
    }
    val hpf     = HPF.ar(chan, 80f)
    val env     = Env.linen(attack = fdIn, sustain = dur - (fdIn + fdOut), release = fdOut, curve = Curve.sine)
    val gain    = "amp".ir(-20.dbamp)
    val eg      = EnvGen.ar(env, levelScale = gain)
    val done    = Done.kr(eg)
//    val limDur  = 0.01f
    val limIn   = hpf * eg
    val lim     = limIn // Limiter.ar(limIn, level = -0.2.dbamp, dur = limDur)
    FreeSelf.kr(done) // TDelay.kr(done, limDur * 2))
    val sig     = lim
    Out.ar(bus, sig)
  }

  private[this] val beeGraphs = Array.tabulate(2)(i => mkBeeGraph(numCh = i + 1))

  private[this] val bees = readBees()

  private[this] val dotIdx = {
    val dotIdx0 = Network.soundDotSeq.indexOf(c.dot)
    if (dotIdx0 < 0) 0 else dotIdx0
  }

  def booted(aural: AuralSystem, s: Server)
            (implicit tx: S#Tx): Unit = {
    log("scsynth booted")

    val ms = Synth.play(masterGraph, nameHint = Some("master"))(target = s.defaultGroup, addAction = addAfter)
    masterSynth() = Some(ms)

    implicit val rnd: Random = new Random
    tx.afterCommit {
      launchBee(left = true )
      launchBee(left = false)
    }
  }

  private def launchBee(left: Boolean)(implicit rnd: Random): Unit = {
    val ch = rnd.nextInt(6) + (if (left) 0 else 6)
    if (!config.isLaptop) {
      relay.selectChannel(ch)
    }
    val bus     = ch / 6
    val beeIdx  = dotIdx * 12 + ch
    val bee     = bees(beeIdx % bees.length)
    val fadeIn  = Util.rrand(10f, 15f)
    val fadeOut = Util.rrand(10f, 15f)
    val dur     = fadeIn + fadeOut + Util.rrand(30f, 60f)
    val start   = Util.rrand(0, bee.numFrames - 44100)
    val amp     = bee.amp * config.beeAmp

    system.step { implicit tx =>
      aural.serverOption.foreach { s =>
        val target  = s.defaultGroup
        val path    = (beeDir / beeFmt.format(bee.id)).path
        val buf     = Buffer.diskIn(s)(path = path, startFrame = start, numChannels = bee.numChannels)
//        val dur     = math.max(0L, stop - start) / Vertex.SampleRate
        // avoid clicking
        val fdIn1   = if (fadeIn  > 0) fadeIn  else 0.01f
        val fdOut1  = if (fadeOut > 0) fadeOut else 0.01f
        val gr      = beeGraphs(bee.numChannels - 1)
        val syn     = Synth.play(gr, nameHint = Some("bee"))(target = target,
          args = List("bus" -> bus, "buf" -> buf.id, "dur" -> dur, "fadeIn" -> fdIn1, "fadeOut" -> fdOut1, "amp" -> amp),
          dependencies = buf :: Nil)
        syn.onEndTxn { implicit tx =>
          buf.dispose()
          tx.afterCommit(launchBee(left = left))
        }
      }
    }
  }
}