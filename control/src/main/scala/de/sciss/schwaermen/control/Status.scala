/*
 *  Status.scala
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
package control

import java.net.SocketAddress

/** @param dot      last byte of the IP address
  * @param version  currently run software version
  * @param update   update progress, or 0.0 if not updating
  */
final case class Status(dot: Int)(val version: String)(val update: Double = 0.0) {
  lazy val pos: Int = Network.dotSeq.indexOf(dot)

  def socketAddress: SocketAddress = Network.dotToSocketMap(dot)

  def copyWithUpdate  (progress: Double): Status = Status(dot)(version)(progress)
  def copyWithVersion (v       : String): Status = Status(dot)(v      )(update  )
}
