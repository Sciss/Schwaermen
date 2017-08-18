package de.sciss.schwaermen
package video

import de.sciss.file._

final case class TrunkPlay(trunkId: Int, delay: Float, start: Float, duration: Float, orientation: Int,
                      fadeIn: Float, fadeOut: Float) {
  override def toString: String = {
    val fd = s"fadeIn = $fadeIn, fadeOut = $fadeOut"
    s"TrunkPlay($trunkId, delay = $delay, start = $start, duration = $duration, orientation = $orientation, $fd)"
  }

  def file(config: Config): File = config.baseDir / "videos" / s"trunk${trunkId}lp.mp4"
}
