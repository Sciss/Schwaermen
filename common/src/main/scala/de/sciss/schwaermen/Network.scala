/*
 *  Network.scala
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

import java.io.FileOutputStream
import java.net.{InetAddress, InetSocketAddress, SocketAddress}
import java.nio.ByteBuffer

import de.sciss.file.file
import de.sciss.osc

object Network {
  /** Maps MAC addresses to IP addresses */
  final val macToIPMap: Map[String, String] = Map(
    // raspberry pi
    "b8:27:eb:71:d5:56" -> "192.168.0.11",
    "b8:27:eb:55:1b:32" -> "192.168.0.12",
    "b8:27:eb:76:1c:85" -> "192.168.0.13",
    "b8:27:eb:37:83:bc" -> "192.168.0.14",
    "b8:27:eb:42:00:49" -> "192.168.0.15",
    "b8:27:eb:72:d1:70" -> "192.168.0.16",
    "b8:27:eb:d9:a5:b9" -> "192.168.0.17",
    "b8:27:eb:c5:19:a6" -> "192.168.0.18",
    "b8:27:eb:36:2e:72" -> "192.168.0.19",
    "b8:27:eb:36:50:58" -> "192.168.0.22",
    "b8:27:eb:85:e5:30" -> "192.168.0.24",
    "b8:27:eb:61:90:b9" -> "192.168.0.25"
  )

  /** 'bottom up', i.e. left arm (outside-to-inside), middle arm, right arm */
  final val dotSeq: Vector[Int] =
    Vector(23, 25, 13, 17, 18, 12, 22, 24, 11)

  final val dotSeqCtl: Vector[Int] = dotSeq :+ 77

  final val ClientPort = 57120

  private def mkSocket(dot: Int): InetSocketAddress = {
    val addr = InetAddress.getByAddress(Array(192.toByte, 168.toByte, 0.toByte, dot.toByte))
    new InetSocketAddress(addr, ClientPort)
  }

  final val socketSeq   : Vector[SocketAddress] = dotSeq    .map(mkSocket)
  final val socketSeqCtl: Vector[SocketAddress] = dotSeqCtl .map(mkSocket)

  final val dotToSocketMap: Map[Int, SocketAddress] = (dotSeqCtl zip socketSeqCtl).toMap
  final val socketToDotMap: Map[SocketAddress, Int] = dotToSocketMap.map(_.swap)

  final val dotToSeqMap: Map[Int, Int] = dotSeq.zipWithIndex.toMap

  def thisIP(): String = {
    import sys.process._
    // cf. https://unix.stackexchange.com/questions/384135/
    val ifConfig    = Seq("ip", "a", "show", "eth0").!!
    val ifConfigPat = "inet "
    val line        = ifConfig.split("\n").map(_.trim).find(_.startsWith(ifConfigPat)).getOrElse("")
    val i0          = line.indexOf(ifConfigPat)
    val i1          = if (i0 < 0) 0 else i0 + ifConfigPat.length
    val i2          = line.indexOf("/", i1)
    if (i0 < 0 || i2 < 0) {
      val local = InetAddress.getLocalHost.getHostAddress
      Console.err.println(s"No assigned IP4 found in eth0! Falling back to $local")
      local
    } else {
      line.substring(i1, i2)
    }
  }

  /** Verifies IP according to `ipMap` and
    * MAC address. If IP doesn't match, tries
    * to edit `/etc/dhcpcd.conf` and reboot.
    * This way, we can clone the Raspberry Pi
    * image, and each machine can configure
    * itself from the identical clone.
    */
  def compareIP(host: String): Unit = {
    import sys.process._
    val macAddress  = Seq("cat", "/sys/class/net/eth0/address").!!.trim
    macToIPMap.get(macAddress).fold[String] {
      Console.err.println(s"Unknown MAC address: $macAddress - not trying to match IP.")
      host
    } { desiredIP =>
      println(s"This computer has MAC address $macAddress and IP $host")
      if (desiredIP != host) {
        val confPath = "/etc/dhcpcd.conf"
        println(s"Designated IP is $desiredIP. Updating /etc/dhcpcd.conf...")
        val header = "interface eth0"
        Seq("cp", confPath, s"$confPath.BAK").!
        val init = io.Source.fromFile(file(confPath)).getLines().toList
          .takeWhile(ln => ln.trim() != header).mkString("\n")
        val tail =
          s"""$header
             |
             |static ip_address=$desiredIP/24
             |static routers=192.168.0.1
             |static domain_name_servers=192.168.0.1
             |""".stripMargin
        val contents = s"$init\n$tail"

        val fOut  = new FileOutputStream(confPath)
        fOut.write(contents.getBytes("UTF-8"))
        fOut.close()
        println("Rebooting...")
        Seq("sudo", "reboot", "now").!
      }
      host
    }
  }

  final val oscCodec: osc.PacketCodec =
    osc.PacketCodec().doublePrecision().packetsAsBlobs()

  final val oscQueryVersion: osc.Message =
    osc.Message("/query", "version")

  object oscReplyVersion {
    def apply(s: String): osc.Message = osc.Message("/info", "version", s)

    def unapply(p: osc.Packet): Option[String] = p match {
      case osc.Message("/info", "version", s: String) => Some(s)
      case _ => None
    }
  }

  object oscUpdateInit {
    def apply(uid: Int, size: Long): osc.Message = osc.Message("/update-init", uid, size)

    def unapply(p: osc.Packet): Option[(Int, Long)] = p match {
      case osc.Message("/update-init", uid: Int, size: Long) => Some((uid, size))
      case _ => None
    }
  }

  object oscUpdateGet {
    def apply(uid: Int, offset: Long): osc.Message = osc.Message("/update-get", uid, offset)

    def unapply(p: osc.Packet): Option[(Int, Long)] = p match {
      case osc.Message("/update-get", uid: Int, offset: Long) => Some((uid, offset))
      case _ => None
    }
  }

  object oscUpdateSet {
    def apply(uid: Int, offset: Long, bytes: ByteBuffer): osc.Message =
      osc.Message("/update-set", uid,offset, bytes)

    def unapply(p: osc.Packet): Option[(Int, Long, ByteBuffer)] = p match {
      case osc.Message("/update-set", uid: Int, offset: Long, bytes: ByteBuffer) => Some((uid, offset, bytes))
      case _ => None
    }
  }

  object oscUpdateError {
    def apply(uid: Int, s: String): osc.Message = osc.Message("/error", "update", uid, s)

    def unapply(p: osc.Packet): Option[(Int, String)] = p match {
      case osc.Message("/error", "update", uid: Int, s: String) => Some((uid, s))
      case _ => None
    }
  }

  object oscUpdateSuccess {
    def apply(uid: Int): osc.Message =
      osc.Message("/update-done", uid)

    def unapply(p: osc.Packet): Option[Int] = p match {
      case osc.Message("/update-done", uid: Int) => Some(uid)
      case _ => None
    }
  }

  final val oscShutdown : osc.Message = osc.Message("/shutdown" )
  final val oscReboot   : osc.Message = osc.Message("/reboot"   )
  final val oscHeart    : osc.Message = osc.Message("/heart"    )

  final val oscDumpFilter: osc.Dump.Filter = { p =>
    p.encodedSize(oscCodec) < 1024
  }
}