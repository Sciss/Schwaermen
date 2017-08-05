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

package de.sciss.schwaermen
package sound

import de.sciss.file.File

object Main extends HasBuildInfo {
  protected val buildInfoPackage = "de.sciss.schwaermen.sound"

  def main(args: Array[String]): Unit = {
    println(s"-- Schwärmen Sound $fullVersion --")
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
      val host = Network.thisIP()
      if (!config.isLaptop) {
        Config.compareIP(host)
      }
      run(host, config)
    }
  }

  def run(host: String, config: Config): Unit = {
    val c = OSCClient(config, host)
    new Heartbeat(c)
  }

  val packageName = "schwaermen-rpi-sound"
}