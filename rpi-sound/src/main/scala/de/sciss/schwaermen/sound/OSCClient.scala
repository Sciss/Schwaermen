/*
 *  OSCClient.scala
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

package de.sciss.schwaermen.sound

import de.sciss.osc
import de.sciss.osc.UDP

/** Undirected pair of transmitter and receiver, sharing the same datagram channel. */
final case class OSCClient(tx: UDP.Transmitter.Undirected, rx: UDP.Receiver.Undirected) {
  /** Sends to all possible targets. */
  def ! (p: osc.Packet): Unit =
    Config.socketSeq.foreach { target =>
      tx.send(p, target)
    }
}