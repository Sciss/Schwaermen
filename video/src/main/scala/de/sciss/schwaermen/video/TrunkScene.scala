/*
 *  TrunkScene.scala
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

import java.net.SocketAddress

import scala.concurrent.stm.InTxn

final class TrunkScene extends Scene.Trunk {

  def init()(implicit tx: InTxn): Unit = ???

  def queryInjection(sender: SocketAddress, uid: Long, meta: TextPathFinder.Meta, ejectVideoId: Int,
                     ejectVertex: Int, expectedDelay: Float)
                    (implicit tx: InTxn): Unit = {
    ???
  }
}