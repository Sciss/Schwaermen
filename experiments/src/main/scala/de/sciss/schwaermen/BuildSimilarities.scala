/*
 *  BuildSimilarities.scala
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

import java.io.{DataInputStream, DataOutputStream, FileOutputStream}

import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.stream.Control
import de.sciss.numbers
import de.sciss.span.Span
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

object BuildSimilarities {
  final case class Word(index: Int, span: Span, text: String, fadeIn: Int = 0, fadeOut: Int = 0) {
    override def toString: String = {
      val fadeInS  = if (fadeIn  == 0) "" else s", fadeIn = $fadeIn"
      val fadeOutS = if (fadeOut == 0) "" else s", fadeOut = $fadeOut"
      s"$productPrefix($index, $span, ${quote(text)}$fadeInS$fadeOutS)"
    }
  }

  object Vertex {
    implicit val Ord: Ordering[Vertex] = Ordering.by((v: Vertex) => (v.textIdx, v.words.head.index))
  }
  final case class Vertex(textIdx: Int, words: List[Word]) {
    def span: Span = Span(words.head.span.start, words.last.span.stop)

    def index     : Int = words.head.index
    def lastIndex : Int = words.last.index

    def fadeIn    : Int = words.head.fadeIn
    def fadeOut   : Int = words.last.fadeOut

    def wordsString: String = words.map(_.text).mkString(" ")

    def quote: String =
      BuildSimilarities.quote(words.map(_.text).mkString(s"$textIdx-$index: ", " ", ""))
  }

  final case class Config(idxA: Int, idxB: Int)

  def main(args: Array[String]): Unit = {
    val default = Config(-1, -1)
    val p = new scopt.OptionParser[Config]("BuildSimilarities") {
      opt[Int] ('a', "first-index")
        .required()
        .action { (v, c) => c.copy(idxA = v) }

      opt[Int] ('b', "second-index")
        .required()
        .action { (v, c) => c.copy(idxB = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      import config._
      val verticesA = readVertices(idxA)
      val verticesB = readVertices(idxB)
      println(s"No. vertices$idxA = ${verticesA.size}; No. vertices$idxB = ${verticesB.size}")
      val fOut = file(s"/data/temp/edges$idxA$idxB.bin")
      if (fOut.exists()) {
        println(s"File $fOut already exists. Not overwriting.")
      } else {
        run(verticesA ++ verticesB, fOut = fOut)
      }
    }
  }

  type SimEdge = Edge[Vertex]

  def audioFileIn(textIdx: Int): File =
    file(s"/data/projects/Schwaermen/audio_work/to_copy/gertrude_text$textIdx.aif")

  final val WORD_COOKIE = 0x576f7264  // "Word"

  def readVertices(textIdx: Int): List[Vertex] = {
    val fin = getClass.getResourceAsStream(s"/text${textIdx}words.bin")
//    val fin = new FileInputStream(f)
    try {
      val din     = new DataInputStream(fin)
      val cookie  = din.readInt()
      require(cookie == WORD_COOKIE)
      val numVertices = din.readShort()
      List.fill(numVertices) {
        val numWords = din.readShort()
        val words = List.fill(numWords) {
          val index     = din.readShort
          val spanStart = din.readLong
          val spanStop  = din.readLong
          val text      = din.readUTF
          val fadeIn    = din.readByte
          val fadeOut   = din.readByte
          Word(index = index, span = Span(spanStart, spanStop), text = text, fadeIn = fadeIn, fadeOut = fadeOut)
        }
        Vertex(textIdx = textIdx, words = words)
      }

    } finally {
      fin.close()
    }
  }

  def run(selection: List[Vertex], fOut: File): Unit = {
    val numComb = selection.combinations(2).size
    println(s"Number of combinations: $numComb")

    import scala.concurrent.ExecutionContext.Implicits.global

    val t: Future[List[SimEdge]] = Future {
      val mapAf = mutable.Map.empty[Int, AudioFile]
      val dos = new DataOutputStream(new FileOutputStream(fOut))

      def copy(textIdx: Int, span: Span, target: File): Unit = {
        val af  = mapAf.getOrElseUpdate(textIdx, AudioFile.openRead(audioFileIn(textIdx)))
        val len = span.length.toInt
        val b   = af.buffer(len)
        af.seek(span.start)
        af.read(b)
        val ch0 = b(0)
        var ch = 0
        while (ch < af.numChannels) {
          val chI = b(ch)
          var i = 0
          while (i < len) {
            ch0(i) += chI(i)
            i += 1
          }
          ch += 1
        }
        val afOut = AudioFile.openWrite(target, AudioFileSpec(numChannels = 1, sampleRate = af.sampleRate))
        try {
          afOut.write(Array(ch0))
        } finally {
          afOut.close()
        }
      }

      def run(): List[SimEdge] = try {
        def loop(rem: List[Vertex], res: List[SimEdge], mapTemp: Map[Vertex, File]): List[SimEdge] =
          rem match {
            case v1 :: tail if tail.nonEmpty =>
              var map = mapTemp

              def getTemp(v: Vertex): File =
                map.getOrElse(v, {
                  val f1 = File.createTemp()
                  val f2 = File.createTemp()
                  val span0 = v.span
                  val span1 = if (v.fadeIn == 0) span0
                    else Span(span0.start + (v.fadeIn * 0.5 * 44100).toLong, span0.stop)
                  val span2 = if (v.fadeOut == 0) span1
                    else Span(span1.start, span1.stop - (v.fadeOut * 0.5 * 44100).toLong)

                  copy(v.textIdx, span2, f1)
                  val g = mkAnalysisGraph(inFile = f1, outFile = f2, name = s"ana $v")
                  val c = Control()
                  c.run(g)
                  Await.result(c.status, Duration.Inf)
                  f1.delete()
                  map += v -> f2
                  f2
                })

              val fileA = getTemp(v1)
              val v1li = v1.lastIndex
              val edge = tail.flatMap {
                case v2 if v2.lastIndex != v1li =>
                  val fileB = getTemp(v2)
                  val name = s"${v1.words.head.index}-${v2.words.head.index}"
                  val (g, fut) = mkCorrelationGraph(fileA = fileA, fileB = fileB, name = name)
                  val c = Control()
                  c.run(g)
                  val sim = Await.result(fut, Duration.Inf)
                  dos.writeByte (v1.textIdx)
                  dos.writeShort(v1.index)
                  dos.writeByte (v2.textIdx)
                  dos.writeShort(v2.index)
                  dos.writeFloat(sim.toFloat)
                  println(f"${v1.quote} -- ${v2.quote} : $sim%g")
                  val e = Edge(v1, v2)(sim): SimEdge
                  Some(e)

                case _ => None
              }
              loop(tail, edge ::: res, map)

            case _ => res
          }

        val res = loop(selection, Nil, Map.empty)
        println("Done.")
        res

      } finally {
        mapAf.valuesIterator.foreach(_.cleanUp())
//        af.cleanUp()
        dos.close()
      }

      run()
    }

    val busy = new Thread {
      override def run(): Unit = synchronized(wait())
      start()
    }

    def release(): Unit = busy.synchronized(busy.notify())

    t.onComplete {
      case Success(edgesI) =>
        val minSim  = edgesI.iterator.map(_.weight).min
        val maxSim  = edgesI.iterator.map(_.weight).max
        println(f"minSim = $minSim%g, maxSim = $maxSim%g")
        import numbers.Implicits._
        val edges   = edgesI.map(e => e.updateWeight(e.weight.linlin(minSim, maxSim, 1.0, 0.0)))

//        implicit val ord: Ordering[Vertex] = Ordering.by(_.words.head.index)
        val mst = MSTKruskal[Vertex, SimEdge](edges)

        def vName(v: Vertex): String = s"v${v.index}"

        val nodesSx = selection.map { v =>
          val label = v.quote
          s"""${vName(v)} [label=$label]"""
        }
        val edgesSx = mst.map { e =>
          f"""${vName(e.start)} -- ${vName(e.end)} [label="${e.weight}%g"]"""
        }
        val nodesS = nodesSx.mkString("  ", "\n  ", "")
        val edgesS = edgesSx.mkString("  ", "\n  ", "")
        val viz =
          s"""graph {
             |$nodesS
             |$edgesS
             |}
             |""".stripMargin

        println(viz)
        release()

      case Failure(ex) => ex.printStackTrace()
        release()
    }
  }

  def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt) else String.valueOf(ch)
  }

  /** Escapes characters such as newlines and quotation marks in a string. */
  def escape(s: String): String = s.flatMap(escapedChar)
  /** Escapes characters such as newlines in a string, and adds quotation marks around it.
    * That is, formats the string as a string literal in a source code.
    */
  def quote (s: String): String = "\"" + escape(s) + "\""

  def mkAnalysisGraph(inFile: File, outFile: File, name: String, fftSize: Int = 1024, winStep: Int = 256,
                      melBands: Int = 64): Graph = {
    val g = Graph {
      import de.sciss.fscape._
      import graph._

//      val spec   = AudioFile.readSpec(inFile)

      val in1  = AudioFileIn(inFile, numChannels = 1)
      val in2  = AudioFileIn(inFile, numChannels = 1)

      def mkEnergy(in: GE): GE = {
        val mx  = RunningSum(in.squared).last
        mx
      }

      def mkMFCC(in: GE): GE = {
        val lap  = Sliding(in, size = fftSize, step = winStep) * GenWindow(fftSize, GenWindow.Hann)
        val fft  = Real1FFT(lap, size = fftSize, mode = 1)
        val mag  = fft.complex.mag // .max(-80)
        val mel  = MelFilter(mag, winStep, bands = melBands, minFreq = 86, maxFreq = 16512)
        mel // DCT_II(mel.log.max(-80), size = melBands, numCoeffs = numMFCC, zero = 0)
        //        val mag  = fft.complex.mag.log.max(-80)
        //        val mel  = MelFilter(mag, winStep, bands = melBands, minFreq = 50, maxFreq = 4000)
        //        DCT_II(mel, melBands, numMFCC, zero = 0)
      }

      val mfcc  = mkMFCC(in1)
      val e     = mkEnergy(in2)
      val sig   = e ++ mfcc
      AudioFileOut(outFile, AudioFileSpec(numChannels = 1, sampleRate = 44100), in = sig)
    }
    g
  }

  def mkCorrelationGraph(fileA: File, fileB: File, name: String, melBands: Int = 64): (Graph, Future[Double]) = {
    val p = Promise[Double]()
    val g = Graph {
      import de.sciss.fscape._
      import de.sciss.numbers.Implicits._
      import graph._

      val specA   = AudioFile.readSpec(fileA)
      val specB   = AudioFile.readSpec(fileB)

      val chunkA  = AudioFileIn(fileA, numChannels = 1)
      val chunkB  = AudioFileIn(fileB, numChannels = 1)
      val energyA = chunkA.head
      val energyB = chunkB.head
      val mfccA   = chunkA.tail
      val mfccB   = chunkB.tail

      val numMFCC   = melBands // 64 // 128 // 42 // 32

      val numWinA   = ((specA.numFrames - 1) / numMFCC).toInt
      val numWinB   = ((specB.numFrames - 1) / numMFCC).toInt

      val lenA2     = numWinA * numMFCC
      val lenB2     = numWinB * numMFCC

      val convLen: Int = numWinA + numWinB - 1

      val fftSize   = (convLen * numMFCC).nextPowerOfTwo
      val chunkAP   = mfccA ++ DC(0).take(fftSize - lenA2)
      val chunkBP   = mfccB ++ DC(0).take(fftSize - lenB2)

      val fftMode   = 1
      val fftA      = Real1FFT(in = chunkAP, size = fftSize, mode = fftMode)
      val fftB0     = Real1FFT(in = chunkBP, size = fftSize, mode = fftMode)
      val fftB      = fftB0.complex.conj

      val prod      = fftA.complex * fftB
      val corr0     = Real1IFFT(in = prod, size = fftSize, mode = fftMode)

      val energyT = (energyA + energyB)/2

      val corr      = corr0

      val max       = RunningMax(corr).last / energyT * (fftSize/2)

      Fulfill(max, p)
    }
    (g, p.future)
  }

  def mkGraph(fileA: File, fileB: File, name: String): (Graph, Future[Double]) = {
    val p = Promise[Double]()
    val g = Graph {
      import de.sciss.fscape._
      import de.sciss.numbers.Implicits._
      import graph._

      val specA   = AudioFile.readSpec(fileA)
      val specB   = AudioFile.readSpec(fileB)
      val lenA    = specA.numFrames
      val lenB    = specB.numFrames

      val chunkA  = AudioFileIn(fileA, numChannels = 1)
      val chunkB  = AudioFileIn(fileB, numChannels = 1)

      //      def normalize(in: GE, len: GE, label: String): GE = {
      //        val inB = in.elastic(len/ControlBlockSize())
      //        val mx  = RunningSum(in.squared).last
      //        mx.poll(0, s"ENERGY OF $label")
      //        inB / mx // (mx / len).sqrt
      //      }

      def energy(in: GE): GE = {
        //        val inB = in.elastic(len/ControlBlockSize())
        val mx  = RunningSum(in.squared).last
        mx
      }

      val fsz       = 1024
      val winStep   = fsz / 4 // fsz/2
      val melBands  = 64 // 128 // 42
      val numMFCC   = 64 // 128 // 42 // 32

      def mfcc(in: GE): GE = {
        val lap  = Sliding(in, size = fsz, step = winStep) * GenWindow(fsz, GenWindow.Hann)
        val fft  = Real1FFT(lap, size = fsz, mode = 1)
        val mag  = fft.complex.mag // .max(-80)
        val mel  = MelFilter(mag, winStep, bands = melBands, minFreq = 86, maxFreq = 16512)
        mel // DCT_II(mel.log.max(-80), size = melBands, numCoeffs = numMFCC, zero = 0)
        //        val mag  = fft.complex.mag.log.max(-80)
        //        val mel  = MelFilter(mag, winStep, bands = melBands, minFreq = 50, maxFreq = 4000)
        //        DCT_II(mel, melBands, numMFCC, zero = 0)
      }

      def numWindows(a: Long, b: Int): Int = {
        //        val c = a + b - 1
        //        val d = c - (c % b)
        //        d / b
        //
        (a.toDouble / b).ceil.toInt + 1
      }

      val numWinA   = numWindows(lenA, winStep)
      val numWinB   = numWindows(lenB, winStep)
      val mfccA     = mfcc(chunkA)
      val mfccB     = mfcc(chunkB)
      //      val mfccBR    = ReverseWindow(mfccB, size = numWinB * numMFCC)

      //      Plot1D(mfccA , size = numWinB * numMFCC)
      //      Plot1D(mfccBR, size = numWinB * numMFCC)

      val lenA2     = numWinA * numMFCC
      val lenB2     = numWinB * numMFCC

      val convLen: Int = numWinA + numWinB - 1
//      println(s"lenA = $lenA, numWinA = $numWinA, lenB =  $lenB, numWinB = $numWinB, winStep = $winStep, convLen = $convLen")

      //      Plot1D(mfccAN, size = lenA2)
      //      Plot1D(mfccBN, size = lenB2)

      //      for (i <- 0 until 4) {
      //        Plot1D(mfccAN.drop(i * numMFCC), size = numMFCC, label = s"mfccAN $i")
      //        Plot1D(mfccBN.drop(i * numMFCC), size = numMFCC, label = s"mfccBN $i")
      //      }

      val fftSize   = (convLen * numMFCC).nextPowerOfTwo
      val chunkAP   = mfccA ++ DC(0).take(fftSize - lenA2) // ResizeWindow(mfccAN, size = lenA2, stop = (convLen - numWinA) * numMFCC)
      val chunkBP   = mfccB ++ DC(0).take(fftSize - lenB2) // ResizeWindow(mfccBN, size = lenB2, stop = (convLen - numWinB) * numMFCC)

      //      lenA2.poll(0, "lenA2")
      //      Length(mfccA  ).poll(0, "mfccA  .length")
      //      (fftSize).poll(0, "fftSize")
      //      (fftSize - lenA2).poll(0, "fftSize - lenA2")
      //      Length(chunkAP).poll(0, "chunkAP.length")

      //      val chunkBP   = ResizeWindow(mfccBN, size = lenB2, start = -(convLen - numWinB) * numMFCC)
      //      val chunkBP   = ReverseWindow(chunkBP0, size = fftSize)

      //      Plot1D(chunkAP, size = convLen * numMFCC, label = "A")
      //      Plot1D(chunkBP, size = convLen * numMFCC, label = "B")

      //      val noise0  = WhiteNoise().take(lenA2) ++ DC(0).take((convLen - numWinA) * numMFCC)
      //      val noise   = normalize(noise0, lenA2)
      //      val noiseR  = noise // ReverseWindow(noise, size = fftSize)

      val fftMode   = 1
      val fftA      = Real1FFT(in = chunkAP, size = fftSize, mode = fftMode)
      val fftB0     = Real1FFT(in = chunkBP, size = fftSize, mode = fftMode)
      val fftB      = fftB0.complex.conj

      //      val testA     = Real1IFFT(in = fftA, size = fftSize, mode = fftMode)
      //      RunningMax(chunkAP.elastic(100) - testA).last.poll(0, "DIFF A")
      //      val testB     = Real1IFFT(in = fftB0, size = fftSize, mode = fftMode)
      //      RunningMax(chunkBP.elastic(100) - testB).last.poll(0, "DIFF B")

      //      Plot1D(fftA.complex.phase, size = fftSize / 2, label = "A")
      //      Plot1D(fftB.complex.phase, size = fftSize / 2, label = "B")

      val prod      = fftA.complex * fftB
      val corr0     = Real1IFFT(in = prod, size = fftSize, mode = fftMode)

      //      Plot1D(prod.complex.mag, size = fftSize / 2, label = "prod-mag")

      //      Plot1D(corr0, size = fftSize, label = "ifft")

      val energyA = energy(mfccA)
      val energyB = energy(mfccB)
      val energyT = (energyA + energyB)/2

      //      val corr      = ResizeWindow(corr0, size = numMFCC, start = 0, stop = 1 - numMFCC)
      val corr      = corr0  // ResizeWindow(corr0, size = numMFCC, start = numMFCC - 1, stop = 0)

      //      Plot1D(corr, size = convLen, label = "corr")

      val max       = RunningMax(corr).last / energyT * (fftSize/2)

      //      val alt = RunningSum(mfccA * mfccB).last
      //      (alt / energyT).poll(0, "alt")

      //      val alt2  = RunningSum(mfccA * mfccB).last
      //      val selfA = RunningSum(mfccA.squared).last
      //      selfA.poll(0, "self-A")
      //      val selfB = RunningSum(mfccB.squared).last
      //      selfB.poll(0, "self-B")
      //      (alt2 / (selfA + selfB)).poll(0, "alt2")
      //
      //      val alt3 = RunningSum((mfccA / selfA) * (mfccB / selfB)).last
      //      (alt3).poll(0, "alt3")

      Fulfill(max, p)
    }
    (g, p.future)
  }

  def mkGraphOLD3(fileA: File, fileB: File, name: String): (Graph, Future[Double]) = {
    val p = Promise[Double]()
    val g = Graph {
      import de.sciss.fscape._
      import de.sciss.numbers.Implicits._
      import graph._

      val specA   = AudioFile.readSpec(fileA)
      val specB   = AudioFile.readSpec(fileB)
      val lenA    = specA.numFrames
      val lenB    = specB.numFrames

      val chunkA = AudioFileIn(fileA, numChannels = 1)
      val chunkB = AudioFileIn(fileB, numChannels = 1)

      def normalize(in: GE, len: GE): GE = {
        val inB = in.elastic(len/ControlBlockSize())
        val mx  = RunningSum(in.squared).last
        inB / (mx / len).sqrt
      }

      val fsz       = 1024
      val winStep   = fsz/2
      val melBands  = 42
      val numMFCC   = 32

      def mfcc(in: GE): GE = {
        val lap  = Sliding(in, fsz, winStep) * GenWindow(fsz, GenWindow.Hann)
        val fft  = Real1FFT(lap, fsz, mode = 1)
        val mag  = fft.complex.mag.max(-80)
        val mel  = MelFilter(mag, winStep, bands = melBands)
        DCT_II(mel.log, melBands, numMFCC, zero = 0)
      }

      def ceilL(a: Long, b: Int): Long = {
        val c = a + b - 1
        c - (c % b)
      }

      val mfccA   = mfcc(chunkA)
      val chunkBR = ReverseWindow(chunkB, size = lenB)
      val mfccB   = mfcc(chunkBR)
      val numWinA = ceilL(lenA, winStep)
      val numWinB = ceilL(lenB, winStep)
      val lenA2   = numWinA * numMFCC
      val lenB2   = numWinB * numMFCC

      val convLen0= numWinA + numWinB - 1
      val convLen = convLen0.toInt.nextPowerOfTwo

      val mfccAN  = normalize(mfccA, lenA2)
      val mfccBN  = normalize(mfccB, lenB2)
      // val lenMin  = lenA2.min(lenB2)

      val chunkAP = ResizeWindow(mfccAN, size = lenA2, stop = (convLen - numWinA) * numMFCC)
      val chunkBP = ResizeWindow(mfccBN, size = lenB2, stop = (convLen - numWinB) * numMFCC)
      val fftMode = 0
      val fftA    = Real2FFT(in = chunkAP, rows = numMFCC, columns = convLen, mode = fftMode)
      val fftB    = Real2FFT(in = chunkBP, rows = numMFCC, columns = convLen, mode = fftMode)
      val prod    = fftA.complex * fftB
      val corr    = Real2IFFT(in = prod  , rows = numMFCC, columns = convLen, mode = fftMode)
      val max     = RunningMax(corr).last //  / /* (2 * lenMin) */ (lenA2 + lenB2) * convLen
      //      max.poll(0, s"similarity ($name)")
      Fulfill(max, p)
    }
    (g, p.future)
  }

  def mkGraphOLD2(fileA: File, fileB: File, name: String): (Graph, Future[Double]) = {
    val p = Promise[Double]()
    val g = Graph {
      import de.sciss.fscape._
      import de.sciss.numbers.Implicits._
      import graph._

      val specA   = AudioFile.readSpec(fileA)
      val specB   = AudioFile.readSpec(fileB)
      val lenA    = specA.numFrames
      val lenB    = specB.numFrames

      val chunkA = AudioFileIn(fileA, numChannels = 1)
      val chunkB = AudioFileIn(fileB, numChannels = 1)

      def normalize(in: GE, len: GE): GE = {
        val inB = in.elastic(len/ControlBlockSize())
        val mx  = RunningSum(in.squared).last
        inB / (mx / len).sqrt
      }

      val mfccA = chunkA
      val mfccB = chunkB
      val lenA2 = lenA
      val lenB2 = lenB

      val convLen0= lenA2 + lenB2 - 1
      val convLen = convLen0.toInt.nextPowerOfTwo

      val mfccAN  = normalize(mfccA, lenA2)
      val mfccBN  = normalize(mfccB, lenB2)
      // val lenMin  = lenA2.min(lenB2)

      val chunkAP = ResizeWindow(mfccAN, size = lenA2, stop = convLen - lenA2)
      val chunkBP = ResizeWindow(mfccBN, size = lenB2, stop = convLen - lenB2)
      val chunkBR = ReverseWindow(in = chunkBP, size = convLen)
      val fftA    = Real1FFT(in = chunkAP, size = convLen, mode = 1)
      val fftB    = Real1FFT(in = chunkBR, size = convLen, mode = 1)
      val prod    = fftA.complex * fftB
      val corr    = Real1IFFT(in = prod, size = convLen, mode = 1)
      val max     = RunningMax(corr).last //  / /* (2 * lenMin) */ (lenA2 + lenB2) * convLen
      //      max.poll(0, s"similarity ($name)")
      Fulfill(max, p)
    }
    (g, p.future)
  }

  def mkGraphOLD(fileA: File, fileB: File, name: String): (Graph, Future[Double]) = {
    val p = Promise[Double]()
    val g = Graph {
      import de.sciss.fscape._
      import de.sciss.numbers.Implicits._
      import graph._

      val specA   = AudioFile.readSpec(fileA)
      val specB   = AudioFile.readSpec(fileB)
      val lenA    = specA.numFrames
      val lenB    = specB.numFrames

      val chunkA = AudioFileIn(fileA, numChannels = 1)
      val chunkB = AudioFileIn(fileB, numChannels = 1)

      def normalize(in: GE, len: GE): GE = {
        val inB = in.elastic(len/ControlBlockSize())
        val mx  = RunningSum(in.squared).last
        inB / (mx / len).sqrt
      }

      val fsz       = 1024
      val winStep   = fsz/2
      val melBands  = 42
      val numMFCC   = 42 // 13
      require(numMFCC == melBands) // we dropped the DCT

      def mfcc(in: GE): GE = {
        val lap  = Sliding(in, fsz, winStep) * GenWindow(fsz, GenWindow.Hann)
        val fft  = Real1FFT(lap, fsz, mode = 1)
        val mag  = fft.complex.mag.max(-80)
        val mel  = MelFilter(mag, winStep, bands = melBands)
        DCT_II(mel.log, melBands, numMFCC, zero = 0)
      }

      def ceilL(a: Long, b: Int): Long = {
        val c = a + b - 1
        c - (c % b)
      }

      val mfccA   = mfcc(chunkA)
      val mfccB   = mfcc(chunkB)
      val numWinA = ceilL(lenA, winStep)
      val numWinB = ceilL(lenB, winStep)
      val lenA2   = numWinA * numMFCC
      val lenB2   = numWinB * numMFCC

      val convLen0= lenA2 + lenB2 - 1
      val convLen = convLen0.toInt.nextPowerOfTwo

      val mfccAN  = normalize(mfccA, lenA2)
      val mfccBN  = normalize(mfccB, lenB2)
//      val lenMin  = lenA2.min(lenB2)

      val chunkAP = ResizeWindow(mfccAN, size = lenA2, stop = convLen - lenA2)
      val chunkBP = ResizeWindow(mfccBN, size = lenB2, stop = convLen - lenB2)
      val chunkBR = ReverseWindow(in = chunkBP, size = convLen)
      val fftA    = Real1FFT(in = chunkAP, size = convLen, mode = 1)
      val fftB    = Real1FFT(in = chunkBR, size = convLen, mode = 1)
      val prod    = fftA.complex * fftB
      val corr    = Real1IFFT(in = prod, size = convLen, mode = 1)
      val max     = RunningMax(corr).last //  / /* (2 * lenMin) */ (lenA2 + lenB2) * convLen
//      max.poll(0, s"similarity ($name)")
      Fulfill(max, p)
    }
    (g, p.future)
  }
}