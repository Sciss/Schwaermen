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

import java.net.InetSocketAddress

import de.sciss.file.File
import de.sciss.kollflitz.Vec

import scala.collection.breakOut
import scala.concurrent.stm.atomic
import scala.util.{Failure, Random, Success, Try}

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
        .text (s"Nominal fps for text rendering (default ${default.fps})")
        .validate { v => if (v > 1 && v <= 60) success else failure(s"Must be 1 < x <= 60") }
        .action { (v, c) => c.copy(fps = v) }

      opt[Double] ("font-size")
        .text (s"Font size for text rendering (default ${default.fontSize})")
        .validate { v => if (v > 1 && v < 320) success else failure(s"Must be 1 < x < 320") }
        .action { (v, c) => c.copy(fontSize = v.toFloat) }

      opt[Double] ("text-vx")
        .text (s"Text horizontal velocity (default ${default.textVX})")
        .validate { v => if (v > 0.01 && v < 40.0) success else failure(s"Must be 0.01 < x < 40.0") }
        .action { (v, c) => c.copy(textVX = v.toFloat) }

      opt[Double] ("text-eject-vy")
        .text (s"Text vertical ejection velocity (default ${default.textEjectVY})")
        .validate { v => if (v > 0.01 && v < 40.0) success else failure(s"Must be 0.01 < x < 40.0") }
        .action { (v, c) => c.copy(textEjectVY = v.toFloat) }

      opt[Double] ("text-pair-lyk")
        .text (s"Text left pairwise spring constant (default ${default.textPairLYK})")
        .validate { v => if (v > 0.001 && v < 1.0) success else failure(s"Must be 0.001 < x < 1.0") }
        .action { (v, c) => c.copy(textPairLYK = v.toFloat) }

      opt[Double] ("text-pair-ryk")
        .text (s"Text right pairwise spring constant (default ${default.textPairRYK})")
        .validate { v => if (v > 0.001 && v < 1.0) success else failure(s"Must be 0.001 < x < 1.0") }
        .action { (v, c) => c.copy(textPairRYK = v.toFloat) }

      opt[Long] ("seed")
        .text (s"Seed for the RNG or -1 to use system clock (default ${default.randomSeed})")
        .action { (v, c) => c.copy(randomSeed = v) }

      opt[Double] ("query-path-delay")
        .text (s"Nominal delay in seconds expected for a injection query reply (default ${default.queryPathDelay})")
        .validate { v => if (v >= 0.0 && v <= 30.0) success else failure(s"Must be 0.0 <= x <= 30.0") }
        .action { (v, c) => c.copy(queryPathDelay = v.toFloat) }

      opt[Unit] ("debug-text")
        .text ("Debug text scene")
        .action { (_, c) => c.copy(debugText = true) }

      opt[Unit] ("log")
        .text ("Enable logging")
        .action { (_, c) => c.copy(log = true) }

      opt[Unit] ("small-window")
        .text ("Use small window instead of full-screen, for debugging purposes.")
        .action { (_, c) => c.copy(smallWindow = true) }

      private def parseSocket(s: String): Either[String, InetSocketAddress] = {
        val arr = s.split(':')
        if (arr.length != 2) Left(s"Must be of format <host>:<port>")
        else parseSocket(arr)
      }

      private def parseSocket(arr: Array[String]): Either[String, InetSocketAddress] = {
        val host = arr(0)
        val port = arr(1)
        Try(new InetSocketAddress(host, port.toInt)) match {
          case Success(addr)  => Right(addr)
          case Failure(ex)    => Left(s"Invalid socket address: $host:$port - ${ex.getClass.getSimpleName}")
        }
      }

      private def parseSocketDot(s: String): Either[String, (InetSocketAddress, Int)] = {
        val arr = s.split(':')
        if (arr.length != 3) Left(s"Must be of format <host>:<port>:<dot>")
        else {
          val dotS = arr(2)
          Try(dotS.toInt) match {
            case Success(dot) =>
              parseSocket(arr).map(socket => (socket,dot))
            case Failure(_) => Left(s"Invalid dot: $dotS - must be an integer")
          }
        }
      }

      opt[String] ("own-socket")
        .text (s"Override own IP address and port; must be <host>:<port> ")
        .validate { v =>
          parseSocket(v).map(_ => ())
        }
        .action { (v, c) =>
          val addr = parseSocket(v).right.get
          c.copy(ownSocket = Some(addr))
        }

      private def validateSockets(vs: Seq[String], useDot: Boolean): Either[String, Unit] =
        ((Right(()): Either[String, Unit]) /: vs) { case (e, v) =>
          e.flatMap { _ => parseSocket(v).map(_ => ()) }
        }

      opt[Seq[String]] ("video-sockets")
        .text (s"Override other video nodes' IP addresses and ports; must be a list of <host>:<port>")
        .validate(validateSockets(_, useDot = false))
        .action { (v, c) =>
          val addr: Vec[InetSocketAddress] = v.map(parseSocket(_).right.get)(breakOut)
          c.copy(otherVideoSockets = addr)
        }

      opt[Seq[String]] ("sound-sockets")
        .text (s"Override sound nodes' IP addresses and ports; must be a list of <host>:<port>:<dot>")
        .validate(validateSockets(_, useDot = true))
        .action { (v, c) =>
          val addr: Map[Int, InetSocketAddress] = v.map(parseSocketDot(_).right.get.swap)(breakOut)
          c.copy(soundSockets = addr)
        }

      opt[Int] ("video-id")
        .text ("Explicit video id. Must be 0, 1, or 2.")
        .validate { v => if (v >= 0 && v <= 2) success else failure("Must be 0, 1, or 2") }
        .action { (v, c) => c.copy(videoId = v) }

      opt[Int] ("dot")
        .text ("Explicit 'dot' (normally the last element of the IP address). Used for transaction ids.")
        .validate { v => if (v >= -1 && v <= 255) success else failure("Must be -1, or 0 to 255") }
        .action { (v, c) => c.copy(dot = v) }

      opt[File]("speakers")
        .text (s"Override default speaker path description. Text file to network description.")
        .action { (f, c) => c.copy(speakerPaths = Some(f)) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      val localSocketAddress = config.ownSocket.getOrElse {
        val host = Network.thisIP()
        if (!config.isLaptop) {
          Network.compareIP(host)
        }
        new InetSocketAddress(host, Network.ClientPort)
      }
      checkConfig(config)
      run(localSocketAddress, config)
    }
  }

  def run(localSocketAddress: InetSocketAddress, config: Config): Unit = {
    if (config.log) Main.showLog = true
    implicit val rnd: Random = new Random(config.randomSeed)
    val c = OSCClient(config, localSocketAddress)
    // new Heartbeat(c)
    val textScene = new TextScene(c)

    atomic { implicit tx =>
      Scene.current() = textScene
      textScene.init()
    }
    c.init()
    new Heartbeat(c)
  }
}