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

import com.pi4j.wiringpi.GpioUtil
import de.sciss.file._

import scala.util.control.NonFatal

object Main extends MainLike {
  protected val pkgLast = "sound"

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

      opt[Unit] ("no-qjackctl")
        .text ("Do not launch QJackCtl")
        .action   { (_, c) => c.copy(qjLaunch = false) }

      opt[String]("qjackctl-preset")
        .text (s"QJackCtl preset name (default: ${default.qjPreset})")
        .action { (f, c) => c.copy(qjPreset = f) }

      opt[File]("qjackctl-patchbay")
        .text (s"QJackCtl patchbay path (default: ${default.qjPatchBay})")
        .action { (f, c) => c.copy(qjPatchBay = f) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      val host = Network.thisIP()

      if (!config.isLaptop) {
        Network.compareIP(host)
        // cf. https://github.com/Pi4J/pi4j/issues/238
        println("Setting up GPIO...")
        try {
          GpioUtil.enableNonPrivilegedAccess()
        } catch {
          case NonFatal(ex) =>
            Console.err.println("Could not enable GPIO access")
            ex.printStackTrace()
        }
      }

      if (!config.isLaptop && config.qjLaunch) {
        println("Launching QJackCtl...")
        import sys.process._
        try {
          // -p preset, -a active patch bay, -s start server
          Seq("qjackctl", "-p", config.qjPreset, "-a", config.qjPatchBay.path, "-s").run()
        } catch {
          case NonFatal(ex) =>
            Console.err.println("Could not start QJackCtl")
            ex.printStackTrace()
        }
      }

      checkConfig(config)
      run(host, config)
    }
  }

  def run(host: String, config: Config): Unit = {
    val c = OSCClient(config, host).init()
    new Heartbeat(c)
    if (!config.isLaptop) {
      try {
        c.relay.bothPins  // lazy, now initialises them
      } catch {
        case NonFatal(ex) =>
          Console.err.println("Could not enable GPIO access")
          ex.printStackTrace()
      }
    }
  }
}