package de.sciss.schwaermen
package video

import de.sciss.file._

final case class Config(
                         baseDir            : File    = userHome/"Documents"/"projects"/"Schwaermen",
                         dumpOSC            : Boolean = false,
                         isLaptop           : Boolean = false,
                         disableEnergySaving: Boolean = true,
                         fps                : Int     = 18,   /* bloody Pi can't do more than this */
                         fontSize           : Float   = 72f,
                         textVX             : Float   = 8f,
                         textEjectVY        : Float   = 12f,
                         textPairLYK        : Float   = 0.038f,
                         textPairRYK        : Float   = 0.030f // 0.015f
)
  extends ConfigLike