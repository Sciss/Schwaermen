/*
 *  Loudness.scala
 *  (Schwaermen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

/*
    quick and dirty translation and adaptation to Scala by H.H.Rutz 20-Aug-2017

    Original copyright below:



Zwicker's Loudness Calculation Method SW Tools - V.Portet - (C) 2010
Partially ported from "Program for calculating loudness according to DIN 45631 (ISO 532B)"
provided by the "Journal of Acoustical Society of Japan" (1991)
(And freely accessible on-line via the Osaka University's Website)

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. The origin of this software must not be misrepresented; you must
   not claim that you wrote the original software.  If you use this
   software in a product, an acknowledgment in the product
   documentation would be appreciated but is not required.

3. Altered source versions must be plainly marked as new different
   versions, and must not be misrepresented as being the original
   software.

4. In case of use of some code parts provided herein to build a
   significantly different project, the resulting project must be
   open source as well, and reference to the original code parts
   must be done within the new project source.


THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


V.Portet - Paris, FR.
vincentportet_95@hotmail.com
Version 0.10 of the 13th of April 2010


 */
package de.sciss.schwaermen

import java.io.{DataOutputStream, FileOutputStream}

import de.sciss.file._
import de.sciss.synth.io.AudioFile

/*       BandSplitter - 1/3rd Octave band analyzer from WAV files, providing normalized SPL tables   *
 *        for Zwicker loudness analysis
 */
object Loudness {

  private[this] final val Q                 = 12.0
  private[this] final val RATE_CONV         = 3509.0

  // Zwicker 1/3rd octave bands
//  private[this] val FR = Array[Int](
//    25,31  ,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500)

  private[this] val FR = Array[Double](
    25,31.5,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500)

  final val BEE_COOKIE = 0x42656573 // "Bees"

  def main(args: Array[String]): Unit = {
    val dir       = file("/data/projects/Schwaermen/audio_work/for_pi/loops/")
    val children  = dir.children(_.name.endsWith("_beesLoop.aif"))
    require(children.size == 108,
      s"Found ${children.size} children instead of 108; excluded: ${(dir.children diff children).map(_.name).mkString("'", "', '", "'")}")
    val sorted    = children.sorted(File.NameOrdering)
    val allGains  = sorted.map { inF =>
      val res = servo(inF, verbose = false)
      println(f"'${inF.name}': boost $res%1.0f dB")
      res
    }
    println(f"min-max gain ${allGains.min}%1.0f dB, ${allGains.max}%1.0f dB")

    val fOut = userHome / "Documents" / "devel" / "Schwaermen" / "sound" / "src" / "main" / "resources" / "bees.bin"
    if (fOut.exists()) {
      println(s"File '$fOut' exists. Not overwriting.")
    } else {
      println(s"Writing meta data file '$fOut'...")
      val fos = new FileOutputStream(fOut)
      try {
        val dos = new DataOutputStream(fos)
        dos.writeInt(BEE_COOKIE)
        dos.writeShort(allGains.size)
        (sorted zip allGains).foreach { case (inF, gain) =>
          val spec  = AudioFile.readSpec(inF)
          val n     = inF.name
          val id    = n.substring(0, n.indexOf('_')).toInt
          dos.writeInt  (id)
          dos.writeShort(spec.numChannels)
          dos.writeInt  (spec.numFrames.toInt)
          dos.writeFloat(gain.toFloat)
        }

      } finally {
        fos.close()
      }
    }
    println("Done.")
  }

  def test(inPath: String): Unit = {
    val LT    = zwickerBands(file(inPath))
    val LT_N  = LT.map(_ + 60.0)
    val info = (FR zip LT_N).map { case (freq, lvl) =>
      f"Freq = $freq Hz => Normalized Level: $lvl%1.1f dB"
    }   .mkString("\n")
    println(info)
    val res = zwicker(LT_N, diffuse = true)
    println(res)
  }

  /** @param  in      the file to analyse
    * @param  spl     the reference of 0 dBFS in decibels
    * @param  diffuse whether to assume diffuse field (`true`) or free field (`false`)
    */
  def apply(in: File, spl: Double, diffuse: Boolean): Value = {
    val lt  = zwickerBands(in)
    val ltN = lt.map(_ + spl)
    val res = zwicker(ltN, diffuse = diffuse)
    res
  }

  def servo(in: File, spl0: Double = 60.0, diffuse: Boolean = true, targetPhon: Double = 50.0,
            verbose: Boolean = true): Double = {
    import de.sciss.numbers.Implicits._
    val lt        = zwickerBands(in)
    val ltN0      = lt.map(_ + spl0)
    var currPhon  = zwicker(ltN0, diffuse = diffuse).phon
    var bestPhon  = currPhon
    var add       = 0.0
    var bestAdd   = add
    val dir       = currPhon < targetPhon
    if (verbose) println(f"First iteration for '${in.name}' with spl $spl0%1.1f dB yields $currPhon%1.1f phon.")
    if (verbose) println(s"Going ${if (dir) "up" else "down"} in 1 dB steps now.")
    val step  = if (dir) 1.0 else -1.0
    do {
      add += step
      val spl   = spl0 + add
      val ltN1  = lt.map(_ + spl)
      currPhon  = zwicker(ltN1, diffuse = diffuse).phon
      if ((targetPhon absdif currPhon) < (targetPhon absdif bestPhon)) {
        bestPhon  = currPhon
        bestAdd   = add
      }
    } while ((currPhon < targetPhon) == dir)

    if (verbose) println(f"Final iteration for '${in.name}' yields spl ${spl0 + add}%1.1f dB (gain $bestAdd%1.0f dB), yielding $bestPhon%1.1f phon.")
    bestAdd
  }

  def zwickerBands(in: File): Array[Double] = {
    val afIn = AudioFile.openRead(in)
    try {
      zwickerBandsBody(afIn)
    } finally {
      afIn.cleanUp()
    }
  }

  private def zwickerBandsBody(afIn: AudioFile): Array[Double] = {
    import math.{exp, max}
    val sampleRate  = afIn.sampleRate

    var pkdEnable   = false
    val numFrames   = afIn.numFrames.toInt
    val enableStart = numFrames / 20
    val enableStop  = numFrames - enableStart

    require(afIn.numChannels <= 2)  // XXX TODO -- generalise over numChannels
    val buf0 = afIn.buffer(numFrames)
    afIn.read(buf0)
    val buf = if (afIn.numChannels == 2) buf0 else Array.fill(2)(buf0(0))

    val LT = new Array[Double](28)

    // Perform 28 steps to define the 28 normalized LT[i]'s...
//    println("\nStep 2: Normalized data, three-band filter and scanning...\n")
    for(passCounter <- 0 until 28) {
      // Initialize 3-band filter (params = f( FR[] ) )
      val f0            = FR(passCounter) // .toDouble
      val K1            = 6.28 * f0 *  Q / sampleRate
      val K3            = 6.28 * f0 / (Q * sampleRate)
      val K2            = 1.0 - K3
      val K4            = 1.0 - exp(-0.9 * f0 / RATE_CONV)
      val ampRatio      = 0.55 / Q
      // Initial conditions
      var UoutL         = buf(0)(0).toDouble
      var UoutR         = buf(1)(0).toDouble
      var U1stL         = UoutL
      var U1stR         = UoutR
      var duL           = 0.0
      var duR           = 0.0
      var pkMaxMidLeft  = Double.NegativeInfinity
      var pkMaxMidRight = Double.NegativeInfinity
      var pkMinMidLeft  = Double.PositiveInfinity
      var pkMinMidRight = Double.PositiveInfinity

      // *** Filter and scan the 3 EQ bands ***
      for (frame <- 0 until numFrames) {
        // Enable / disable peak detectors
        pkdEnable = frame > enableStart && frame < enableStop

        // *** Left ch. ***
        val UinL = buf(0)(frame).toDouble
        // Start resonant filter differential equation
        duL += (K1 * (UinL - UoutL))
        UoutL = (K2 * UoutL) + (K3 * duL)
        // Start first order filter U1st = f(Uin)
        U1stL += (K4*(UinL - U1stL))

        if(pkdEnable) {
          // Substract U1st to resonator output (improve rejection at LF and no effect in HF)
          val temp = ampRatio * (UoutL - U1stL)

          // Peak detectors
          if (temp > pkMaxMidLeft) {
            pkMaxMidLeft = temp
          }
          if (temp < pkMinMidLeft) {
            pkMinMidLeft = temp
          }
        }

        // *** Right ch. ***
        val UinR = buf(1)(frame).toDouble
        // Start resonant filter differential equation
        duR += (K1 * (UinR - UoutR))
        UoutR = (K2 * UoutR) + (K3 * duR)
        // Start first order filter U1st = f(Uin)
        U1stR += (K4*(UinR - U1stR))

        if(pkdEnable) {
          // Substract U1st to resonator output (improve rejection at LF and no effect in HF)
          val temp = ampRatio * (UoutR - U1stR)
          // Peak detectors
          if (temp > pkMaxMidRight) {
            pkMaxMidRight = temp
          }
          if (temp < pkMinMidRight) {
            pkMinMidRight = temp
          }
        }
      }
      // Convert peak-peak amplitude to LT[]
      if(pkMaxMidRight==pkMinMidRight) {
        pkMaxMidRight = pkMinMidRight + 1
      }
      if(pkMaxMidLeft == pkMinMidLeft) {
        pkMaxMidLeft = pkMinMidLeft + 1
      }
      // Returns normalized amplitudes (0 dB corresonds to mid-amp)
      import de.sciss.numbers.Implicits._
      LT(passCounter) = max(
        (pkMaxMidLeft  - pkMinMidLeft ).ampdb,
        (pkMaxMidRight - pkMinMidRight).ampdb
      )
//      printf("Freq = %d Hz => Normalized Level: %f dB\n",FR(passCounter),LT(passCounter))
    }
    LT
  }

  // CONSTANT TABLES

  // Center frequencies of 1/3 oct. bands (FR)

  // Ranges of 1/3 oct. levels for correction at low frequencies according to equal loudness contours (RAP)
  private[this] val RAP = Array[Double](45,55,65,71,80,90,100,120)

  // Reduction of 1/3 oct. band levels at low frequencies according to equal loudness contours within the eight ranges defined by RAP (DLL)
  private[this] val DLL = Array[Array[Double]](
    Array[Double](-32,-24,-16,-10,-5,0,-7,-3,0,-2,0),Array[Double](-29,-22,-15,-10,-4,0,-7,-2,0,-2,0),
    Array[Double](-27,-19,-14, -9,-4,0,-6,-2,0,-2,0),Array[Double](-25,-17,-12, -9,-3,0,-5,-2,0,-2,0),
    Array[Double](-23,-16,-11, -7,-3,0,-4,-1,0,-1,0),Array[Double](-20,-14,-10, -6,-3,0,-4,-1,0,-1,0),
    Array[Double](-18,-12, -9, -6,-2,0,-3,-1,0,-1,0),Array[Double](-15,-10, -8, -4,-2,0,-3,-1,0,-1,0)
  )

  // Critical band level at absolute threshold without taking into account the transmission characteristics of the ear (LTQ)
  private[this] val LTQ = Array[Double](30,18,12,8,7,6,5,4,3,3,3,3,3,3,3,3,3,3,3,3)

  // Correction of levels according to the transmission characteristics of the ear (AO)
  private[this] val AO = Array[Double](0,0,0,0,0,0,0,0,0,0,-0.5,-1.6,-3.2,-5.4,-5.6,-4,-1.6,2,5,12)

  // Level difference between free and diffuse sound fields (DDF)
  private[this] val DDF = Array[Double](0,0,0.5,0.9,1.2,1.6,2.3,2.8,3,2,0,-1.4,-2,-1.9,-1,0.5,3,4,4.3,4)

  // Adaptation of 1/3 oct. band levels to the corresponding critical band level (DCB)
  private[this] val DCB = Array[Double](-0.25,-0.6,-0.8,-0.8,-0.5,0,0.5,1.1,1.5,1.7,1.8,1.8,1.7,1.6,1.4,1.2,0.8,0.5,0,-0.5)

  // Upper limits of approximated critical bands in terms of critical band rate (ZUP)
  private[this] val ZUP = Array[Double](0.9,1.8,2.8,3.5,4.4,5.4,6.6,7.9,9.2,10.6,12.3,13.6,15.2,16.7,16.1,19.3,20.6,21.8,22.7,23.6,24.0)

  // Range of specific loudness for the determination of the steepness of the upper slopes in the specific loudness
  // Critical band rate pattern (RNS)
  private[this] val RNS = Array[Double](21.5,18,15.1,11.5,9,6.1,4.4,3.1,2.13,1.36,0.82,0.42,0.30,0.22,0.15,0.1,0.035,0)

  // Steepness of the upper slopes in the specific loudness - Critical band rate pattern for the ranges RNS as a function of the number
  // of the critical band (USL)
  private[this] val USL = Array[Array[Double]](
    Array(13 ,9  ,7.8,6.2,4.5,3.7 ,2.9,2.4 ,1.95,1.5 ,0.72,0.59,0.40,0.27,0.16,0.12,0.09,0.06),
    Array(8.2,7.5,6.7,5.4,3.8,3   ,2.3,1.7 ,1.45,1.2 ,0.07,0.53,0.33,0.21,0.16,0.11,0.08,0.05),
    Array(6.3,6  ,5.6,4.6,3.6,2.8 ,2.1,1.5 ,1.3 ,0.94,0.64,0.51,0.26,0.2 ,0.14,0.1 ,0.07,0.03),
    Array(5.5,5.1,4.8,4  ,3.2,2.35,1  ,1.35,1.15,0.86,0.63,0.5 ,0.24,0.18,0.12,0.08,0.06,0.02),
    Array(5.5,4.5,4.4,3.5,2.9,2.2 ,1.8,1.3 ,1.1 ,0.82,0.62,0.42,0.22,0.17,0.11,0.08,0.06,0.02),
    Array(5.5,4.5,3.9,3.2,2.7,2.2 ,1.7,1.3 ,1.1 ,0.82,0.62,0.42,0.22,0.17,0.11,0.08,0.06,0.02),
    Array(5.5,4.5,3.9,3.2,2.7,2.2 ,1.7,1.3 ,1.1 ,0.82,0.62,0.42,0.22,0.17,0.11,0.08,0.06,0.02),
    Array(5.5,4.5,3.9,3.2,2.7,2.2 ,1.7,1.3 ,1.1 ,0.82,0.62,0.42,0.22,0.17,0.11,0.08,0.05,0.02)
  )

  def zwicker(LT: Array[Double], diffuse: Boolean): Value = {
    import math.{log, log10, max, min, pow}

    val GI  = new Array[Double](  3)
    val LE  = new Array[Double]( 21)
    val LCB = new Array[Double](  3)
    val NM  = new Array[Double]( 21)
    val NS  = new Array[Double](270)
    val TI  = new Array[Double]( 11)

    val S   = 0.25
    var XP  = 0.0

    // Correction of 1/3 oct. band levels according to equal loudness contours (XP) and calculation of the intensities for 1/3 oct. bands up to 315Hz
    for (i <- 0 until 11) {
      var j = 0
      if (LT(i) <= (RAP(j) - DLL(j)(i))) {
        XP = LT(i) + DLL(j)(i)
        TI(i) = pow(10.0, 0.1 * XP)
      } else {
        j += 1
        if(j >= 7) {
          XP = LT(i) + DLL(j)(i)
          TI(i) = pow(10.0, 0.1 * XP)
        } else {
          var flagOut = false
          while (j < 7 && !flagOut) {
            if (LT(i) <= RAP(j) - DLL(j)(i)) {
              XP=LT(i) + DLL(j)(i)
              TI(i) = pow(10.0, 0.1 * XP)
              flagOut=true
            } else {
              j += 1
            }
          }
          if (!flagOut) {
            XP=LT(i) + DLL(j)(i)
            TI(i)=pow(10.0, 0.1 * XP)
          }
        }
      }
    }

    // Determination of levels LCB(0-2) within the first three critical bands
    GI(0) = TI(0) + TI(1) + TI(2) + TI(3) + TI(4) + TI(5)
    GI(1) = TI(6) + TI(7) + TI(8)
    GI(2) = TI(9) + TI(10)

    for(i <- 0 until 3) {
      if (GI(i) > 0.0000001) {
        LCB(i) = 10.0 * log10(GI(i))
      } else {
        LCB(i) = -70.0
      }
    }

    // Calculation of main loudness
    for(i <- 0 until 20) {
      LE(i) = LT(i + 7)
      if (i <= 2) {
        LE(i) = LCB(i)
      }
      LE(i) -= AO(i)
      NM(i) = 0.0
      if (diffuse) {
        LE(i) += DDF(i)
      }
      if (LE(i) > LTQ(i)) {
        LE(i) -= DCB(i)
        // S = .25
        val MP1 = 0.0635 * pow(10.0, 0.025 * LTQ(i))
        val MP2 = pow(1.0 - S + S * pow(10.0, 0.1 * (LE(i) - LTQ(i))), 0.25) - 1.0
        NM(i) = max(0.0, MP1 * MP2)
      }
    }
    NM(20) = 0

    // Correction of specific loudness in the lowest critical band taking into account the dependence of absolute threshold within this critical band
    val KORRY = min(1.0, 0.4 + 0.32 * pow(NM(0), 0.2))
    NM(0) *= KORRY

    // Start values
    var N  = 0.0
    var Z1 = 0.0
    var N1 = 0.0
    var IZ = 0
    var Z  = 0.1

    var Z2 = 0.0
    var N2 = 0.0
    var DZ = 0.0
    var K  = 0.0

    val IZ_MAX = 0 // XXX TODO --- this is never adjusted in the original algorithm, probably a bug

    var j = 0

    // Step to first and subsequent critical bands
    for(i <- 0 until 21) {

      ZUP(i) += 0.0001
      val IG = min(7, i - 1)

      do {
        if (N1 <= NM(i)) {
          if (N1 != NM(i)) {
            // Determination of the number J corresponding to the range of specific loudness
            j = 0
            var flagIn = true
            while(flagIn && j < 18) {
              if (RNS(j) < NM(i)) {
                flagIn = false
              } else {
                j += 1
              }
            }
          }

          // Contribution of unmasked main loudness to total loudness and calculation of values NS(IZ) with a spacing of Z = IZ * 0.1 BARK
          Z2 = ZUP(i)
          N2 = NM(i)
          N += (N2 * (Z2 - Z1))
          K = Z
          while (K < (Z2 + 0.1)) {
            NS(IZ) = N2
            if(IZ < 269) {
              IZ += 1
            } else {
              println("WARNING ! NS Table overflows during calculation of the contribution of unmasked loudness !")
            }
            K += 0.1
          }
          K = Z2 + 0.1
          Z = K
          IZ = IZ_MAX

        } else {
          // Decision whether the critical band in question is completely or partly masked by accessory loudness
          N2 = RNS(j)
          if (N2 < NM(i)) {
            N2 = NM(i)
          }
          DZ = (N1 - N2) / USL(IG)(j)
          Z2 = Z1 + DZ
          if (Z2 > ZUP(i)) {
            Z2 = ZUP(i)
            DZ = Z2 - Z1
            N2 = N1 - (DZ * USL(IG)(j))
          }
          // Contribution of accessory loudness to total loudness
          N += (DZ * (N1 + N2)/2.0)
          K = Z
          while (K < Z2 + 0.1) {
            NS(IZ) = N1 - ((K - Z1) * USL(IG)(j))
            if(IZ < 269) {
              IZ += 1
            } else {
              println("WARNING ! NS Table overflows during calculation of the contribution of accessory loudness !")
            }
            K+=0.1
          }
          K = Z2+0.1
          Z = K
          IZ = IZ_MAX
        }

        // Step to next segment
        while (N2 <= RNS(j) && j < 17) {
          j += 1
        }
        if (j >= 17 && N2 <= RNS(j)) {
          j = 17
        }
        Z1 = Z2
        N1 = N2
      }
      while (Z1 < ZUP(i))
    }

    // End of critical band processing
    if (N < 0.0) {
      N = 0.0
    }
    // Calculation of loudness level for LN < 40 Phon or N < 1 Sone
    var LN = max(3.0, 40.0 * pow(N + 0.0005, 0.35))

    // Calculation of loudness level for LN >= 40 Phon or N >= 1 Sone
    if (N >= 1.0) {
      LN = (10.0 * log(N) / log(2)) + 40.0
    }

    // ---------------------------------------------------------

//    printf("\nRESULTS:\n")
//    printf("-------\n")
//    val MS = if (diffuse) 'D' else 'F'
//    printf("\nType of sound field : %c\n", MS)
//    printf("\nLoudness  N : %.2f Sones G%c\n",N,MS)
//    printf("\nLoudness Level  LN : %.2f Phons G%c\n",LN,MS)
//    printf("\n\n")

    Value(sones = N, phon = LN, diffuse = diffuse)
  }

  final case class Value(sones: Double, phon: Double, diffuse: Boolean) {
    override def toString: String = {
      val MS = if (diffuse) 'D' else 'F'
      f"Loudness(value = $sones%1.2f Sones G$MS, level = $phon%1.2f Phon G$MS"
    }
  }
}
