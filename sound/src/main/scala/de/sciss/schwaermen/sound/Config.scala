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

package de.sciss.schwaermen
package sound

import java.io.FileOutputStream

import de.sciss.file._

object Config {
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
    Network.macToIPMap.get(macAddress).fold[String] {
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