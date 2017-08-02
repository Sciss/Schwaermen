package de.sciss.schwaermen

import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.stream.Control
import de.sciss.synth.io.AudioFile

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

object TestCross {
  def main(args: Array[String]): Unit = {
    val pathA = "/data/temp/corr-in-sichM.aif"
    val pathB = pathA // "/data/temp/corr-in-ueberwurfM.aif"

    val (g, fut) = mkGraph(fileA = file(pathA), fileB = file(pathB))
    val c = Control()
    c.run(g)
    val sim = Await.result(fut, Duration.Inf)
    println(f"sim = $sim%g")
  }

  def mkGraph(fileA: File, fileB: File): (Graph, Future[Double]) = {
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
        val mel  = MelFilter(mag, winStep, bands = melBands, minFreq = 100, maxFreq = 4000)
        DCT_II(mel.log, melBands, numMFCC, zero = 0)
      }

      def ceilL(a: Long, b: Int): Long = {
        val c = a + b - 1
        c - (c % b)
      }

      val numWinA   = ceilL(lenA, winStep)
      val numWinB   = ceilL(lenB, winStep)
      val mfccA     = mfcc(chunkA)
      val mfccB     = mfcc(chunkB)
      val mfccBR    = ReverseWindow(mfccB, size = numWinB * numMFCC)
      val lenA2     = numWinA * numMFCC
      val lenB2     = numWinB * numMFCC

      val convLen0  = numWinA + numWinB - 1
      val convLen   = convLen0.toInt.nextPowerOfTwo

      val mfccAN    = normalize(mfccA, lenA2)
      val mfccBN    = normalize(mfccB, lenB2)

      for (i <- 0 until 4) {
        Plot1D(mfccAN.drop(i * numMFCC), size = numMFCC, label = s"mfccAN $i")
        Plot1D(mfccBN.drop(i * numMFCC), size = numMFCC, label = s"mfccBN $i")
      }

      val chunkAP   = ResizeWindow(mfccAN, size = lenA2, stop = (convLen - numWinA) * numMFCC)
      val chunkBP   = ResizeWindow(mfccBN, size = lenB2, stop = (convLen - numWinB) * numMFCC)
      val fftMode   = 1
      val fftA      = Real1FFT(in = chunkAP, size = numMFCC * convLen, mode = fftMode)
      val fftB      = Real1FFT(in = chunkBP, size = numMFCC * convLen, mode = fftMode)
      val prod      = fftA.complex * fftB
      val corr0     = Real1IFFT(in = prod  , size = numMFCC * convLen, mode = fftMode)
      val corr      = ResizeWindow(corr0, size = numMFCC, start = 0, stop = 1 - numMFCC)
      val max       = RunningMax(corr).last

      Fulfill(max, p)
    }
    (g, p.future)
  }
}
