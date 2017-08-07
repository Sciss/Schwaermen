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
package video

import de.sciss.file.File

import scala.swing.Swing

object Main extends MainLike {
  protected val pkgLast = "video"

  def main(args: Array[String]): Unit = {
    println(s"-- $name $fullVersion --")
    val default = Config()
    val p = new scopt.OptionParser[Config](namePkg) {
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

      opt[Unit] ("keep-energy")
        .text ("Do not turn off energy saving")
        .action   { (_, c) => c.copy(disableEnergySaving = false) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      val host = Network.thisIP()
      if (!config.isLaptop) {
        Network.compareIP(host)
//        // cf. https://github.com/Pi4J/pi4j/issues/238
//        try {
//          GpioUtil.enableNonPrivilegedAccess()
//        } catch {
//          case NonFatal(ex) =>
//            Console.err.println("Could not enable GPIO access")
//            ex.printStackTrace()
//        }
      }
      checkConfig(config)
      run(host, config)
    }
  }

  def run(host: String, config: Config): Unit = {
    /* val c = */ OSCClient(config, host)
    // new Heartbeat(c)
    Swing.onEDT {
      TestRotation.run()
    }
  }
}