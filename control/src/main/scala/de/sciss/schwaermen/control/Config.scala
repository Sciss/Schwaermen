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

package de.sciss.schwaermen.control

import java.net.{InetAddress, InetSocketAddress, SocketAddress}

object Config {
  /** 'bottom up', i.e. left arm (outside-to-inside), middle arm, right arm */
  final val ipSeq: Vector[Int] =
    Vector(23, 25, 17, 13, 18, 12, 22, 24, 11)

  final val ClientPort = 57120

  private def mkSocket(dot: Int): InetSocketAddress = {
    val addr = InetAddress.getByAddress(Array(192.toByte, 168.toByte, 0.toByte, dot.toByte))
    new InetSocketAddress(addr, ClientPort)
  }

  final val socketSeq   : Vector[SocketAddress] = ipSeq   .map(mkSocket)
  final val socketSeqCtl: Vector[SocketAddress] = socketSeq :+ mkSocket(77)

  def thisIP(): String = {
    import sys.process._
    val ifConfig    = Seq("/sbin/ifconfig", "eth0").!!
    val ifConfigPat = "inet addr:"
    val i0          = ifConfig.indexOf(ifConfigPat)
    val i1          = if (i0 < 0) 0 else i0 + ifConfigPat.length
    val i2          = ifConfig.indexOf(" ", i1)
    if (i0 < 0 || i2 < 0) {
      val local = InetAddress.getLocalHost.getHostAddress
      Console.err.println(s"No assigned IP4 found in eth0! Falling back to $local")
      local
    } else {
      ifConfig.substring(i1, i2)
    }
  }
}
final case class Config()