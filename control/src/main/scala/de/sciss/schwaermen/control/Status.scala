package de.sciss.schwaermen.control

/** @param pos      position (bottom up) in the tower, or -1 for laptop
  * @param dot      last byte of the IP address
  * @param version  currently run software version
  * @param update   update progress, or 0.0 if not updating
  */
final case class Status(pos: Int, dot: Int, version: String, update: Double)

