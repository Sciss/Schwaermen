package de.sciss.schwaermen
package video

import de.sciss.file._

final case class Config(
                         baseDir            : File    = userHome/"Documents"/"projects"/"Schwaermen",
                         dumpOSC            : Boolean = false,
                         isLaptop           : Boolean = false,
                         disableEnergySaving: Boolean  = true
                       )
  extends ConfigLike