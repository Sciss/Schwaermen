package de.sciss.schwaermen

import de.sciss.file._

object CatalogTestRender {
  def main(args: Array[String]): Unit = {
    for (i <- 5 to 100 by 5) {
      val fGNG = file(s"/data/temp/catalog_cover-$i.gng")
      val fImg = fGNG.replaceExt("png")
      if (fGNG.length > 0L && fImg.length() == 0L) {
        CatalogCover.render(fGNG, fImg)
      }
    }
  }
}
