/*
 *  Config.scala
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
package video

import java.net.InetSocketAddress

import de.sciss.file._
import de.sciss.kollflitz.Vec

/**
  * @param baseDir              base directory within which resources such as the video files will be found
  * @param dumpOSC              if `true`, print incoming and outgoing OSC packets
  * @param isLaptop             if `true`, assume a test run from laptop, no GPIO etc.
  * @param disableEnergySaving  if `true`, disable screen blanking and power saving on the RPi
  * @param randomSeed           random number generator seed, or `-1`
  * @param fps                  nominal frames per second (for the text state)
  * @param fontSize             font size for the text rendering
  * @param textVX               text horizontal velocity (pixels per frame)
  * @param textEjectVY          text vertical ejection velocity (pixels per frame)
  * @param textPairLYK          text pairwise spring force constant left-hand-side
  * @param textPairRYK          text pairwise spring force constant right-hand-side
  * @param textMinDur           text state minimum 'idle' duration in seconds
  * @param textMaxDur           text state maximum 'idle' duration in seconds
  * @param ownSocket            optional socket to bind local OSC client to (useful when debugging from laptop)
  * @param otherVideoSockets    override list of video nodes' socket addresses (useful when debugging from laptop)
  * @param videoId              override video id (0 to 2) (useful when debugging from laptop)
  * @param dot                  override 'dot' transaction id (useful when debugging from laptop)
  * @param log                  enable log message printing
  * @param smallWindow          open only small video window instead of full-screen (useful when debugging)
  * @param queryPathDelay       nominal delay in seconds expected for a injection query reply
  */
final case class Config(
                        baseDir             : File    = userHome/"Documents"/"projects"/"Schwaermen",
                        dumpOSC             : Boolean = false,
                        isLaptop            : Boolean = false,
                        disableEnergySaving : Boolean = true,
                        randomSeed          : Long    = -1L,
                        fps                 : Int     = 18,   /* bloody Pi can't do more than this */
                        fontSize            : Float   = 72f,
                        textVX              : Float   = 8f,
                        textEjectVY         : Float   = 12f,
                        textPairLYK         : Float   = 0.038f,
                        textPairRYK         : Float   = 0.030f, // 0.015f
                        textMinDur          : Float   = 30f,
                        textMaxDur          : Float   = 60f,
                        debugText           : Boolean = false,
                        ownSocket           : Option[InetSocketAddress] = None,
                        videoId             : Int     = -1,
                        dot                 : Int     = -1,
                        otherVideoSockets   : Vec[InetSocketAddress] = Vector.empty,
                        log                 : Boolean = false,
                        smallWindow         : Boolean = false,
                        queryPathDelay      : Float   = 2.5f
)
  extends ConfigLike