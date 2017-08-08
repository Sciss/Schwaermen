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
        .action { (_, c) => c.copy(disableEnergySaving = false) }

      opt[Int] ("fps")
        .text (s"Nominal fps for text rendering (default ${default.fps}")
        .validate { v => if (v > 1 && v <= 60) success else failure(s"Must be 1 < fps <= 60") }
        .action { (v, c) => c.copy(fps = v) }

      opt[Double] ("font-size")
        .text (s"Font size for text rendering (default ${default.fontSize}")
        .validate { v => if (v > 1 && v < 320) success else failure(s"Must be 1 < fps < 320") }
        .action { (v, c) => c.copy(fontSize = v.toFloat) }

      opt[Double] ("text-vx")
        .text (s"Text horizontal velocity (default ${default.textVX}")
        .validate { v => if (v > 0.01 && v < 40.0) success else failure(s"Must be 0.01 < fps < 40.0") }
        .action { (v, c) => c.copy(textVX = v.toFloat) }

      opt[Double] ("text-eject-vy")
        .text (s"ext vertical ejection velocity (default ${default.textEjectVY}")
        .validate { v => if (v > 0.01 && v < 40.0) success else failure(s"Must be 0.01 < fps < 40.0") }
        .action { (v, c) => c.copy(textEjectVY = v.toFloat) }
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
      View.run(config)
    }
  }
}