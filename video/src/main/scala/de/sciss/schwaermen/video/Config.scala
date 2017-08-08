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
                         textVX             : Float   = 4f,
                         textEjectVY        : Float   = 8f
                       )
  extends ConfigLike