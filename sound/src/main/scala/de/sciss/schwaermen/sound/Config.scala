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
package sound

import de.sciss.file._

final case class Config(
                        baseDir : File    = userHome/"Documents"/"projects"/"Schwaermen",
                        dumpOSC : Boolean = false,
                        isLaptop: Boolean = false
                       )
  extends ConfigLike