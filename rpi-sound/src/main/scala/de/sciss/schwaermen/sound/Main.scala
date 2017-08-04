/*
 *  Main.scala
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

import java.net.InetSocketAddress

import de.sciss.file.File
import de.sciss.osc.UDP

import scala.util.control.NonFatal

object Main {
  private def buildInfString(key: String): String = try {
    val clazz = Class.forName("de.sciss.schwaermen.sound.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(_) => "?"
  }

  def main(args: Array[String]): Unit = {
    println(s"-- Schwärmen Sound v${buildInfString("version")}, built ${buildInfString("builtAtString")} --")
    val default = Config()
    val p = new scopt.OptionParser[Config]("Imperfect-RaspiPlayer") {
      opt[File]("base-dir")
        .text (s"Base directory (default: ${default.baseDir})")
        // .required()
        .action { (f, c) => c.copy(baseDir = f) }

      opt[Unit] ('d', "dump-osc")
        .text (s"Enable OSC dump (default ${default.dumpOSC})")
        .action { (_, c) => c.copy(dumpOSC = true) }

      opt[Unit] ("laptop")
        .text (s"Instance is laptop (default ${default.isLaptop})")
        .action { (_, c) => c.copy(isLaptop = true) }

//      opt[Unit] ("keep-energy")
//        .text ("Do not turn off energy saving")
//        .action   { (_, c) => c.copy(disableEnergySaving = false) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      val host = Config.thisIP()
      if (!config.isLaptop) {
        Config.compareIP(host)
      }
      run(host, config)
    }
  }

  def run(host: String, config: Config): Unit = {
    val c = mkClient(host)
    new Heartbeat(c)
  }

  def mkClient(host: String): OSCClient = {
    val c = UDP.Config()
    // c.codec
    c.localSocketAddress = new InetSocketAddress(host, Config.ClientPort)
    val tx  = UDP.Transmitter(c)
    val rx  = UDP.Receiver(tx.channel, c)
    tx.connect()
    rx.connect()
    OSCClient(tx, rx)
  }
}