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

package de.sciss.schwaermen.sound

import java.io.FileOutputStream
import java.net.{InetAddress, InetSocketAddress, SocketAddress}

import de.sciss.file._

object Config {
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
    "???"               -> "192.168.0.25"
  )

  /** 'bottom up', i.e. left arm (outside-to-inside), middle arm, right arm */
  final val ipSeq: Vector[Int] =
    Vector(23, 25, 17, 13, 18, 12, 22, 24, 11)

  final val ClientPort = 57120

  final val socketSeq: Vector[SocketAddress] = ipSeq.map { dot =>
    val addr = InetAddress.getByAddress(Array(192.toByte, 168.toByte, 0.toByte, dot.toByte))
    new InetSocketAddress(addr, ClientPort)
  }

  final val ipToSeqMap: Map[Int, Int] = ipSeq.zipWithIndex.toMap

  final val boardPins: Map[Int, RelayPins] = Map(
    12 -> RelayPins.default.replace(now =  4, before =  3),
    13 -> RelayPins.default.replace(now =  8, before = 11),
    17 -> RelayPins.default.replace(now =  4, before = 15),
    18 -> RelayPins(left = Vector(2, 4, 14, 15, 17, 18), right = Vector(22, 23, 9, 24, 10, 8)),
    22 -> RelayPins.default.replace(now = 27, before = 18),
    24 -> RelayPins.default.replace(now = 27, before = 18)
  )   .withDefaultValue(RelayPins.default)

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
}
final case class Config(
                        baseDir : File    = userHome/"Documents"/"projects"/"Schwaermen",
                        dumpOSC : Boolean = false,
                        isLaptop: Boolean = false
                       )