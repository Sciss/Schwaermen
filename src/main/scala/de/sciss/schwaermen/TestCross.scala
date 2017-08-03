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
    val pathB = "/data/temp/corr-in-ueberwurfM.aif"

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
      val winStep   = fsz/2
      val melBands  = 42
      val numMFCC   = 42 // 32

      def mfcc(in: GE): GE = {
        val lap  = Sliding(in, size = fsz, step = winStep) * GenWindow(fsz, GenWindow.Hann)
        val fft  = Real1FFT(lap, size = fsz, mode = 1)
        val mag  = fft.complex.mag // .max(-80)
        val mel  = MelFilter(mag, winStep, bands = melBands, minFreq = 80, maxFreq = 14000)
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
      println(s"lenA = $lenA, numWinA = $numWinA, lenB =  $lenB, numWinB = $numWinB, winStep = $winStep, convLen = $convLen")

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
}
