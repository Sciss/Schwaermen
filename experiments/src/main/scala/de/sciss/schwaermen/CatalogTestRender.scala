/*
 *  CatalogTestRender.scala
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

package de.sciss.schwaermen

import de.sciss.file._

object CatalogTestRender {
  def main(args: Array[String]): Unit = {
    val dir   = file(s"/data/temp")
    val gngIn = dir.children { f =>
      f.name.startsWith("catalog_coverM-") && f.ext == "gng" && f.length() > 0L
    }

    val c = CatalogCover.Config()

//    for (i <- 5 to 100 by 5) {
    gngIn.foreach { fGNG =>
//      val fGNG = file(s"/data/temp/catalog_cover-$i.gng")
      val fImg = fGNG.replaceExt("png")
      if (fGNG.length > 0L && fImg.length() == 0L) {
        CatalogCover.renderImage(c)(fGNG, fImg)
      }
    }
  }
}
