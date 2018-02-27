/*
 *  Fulfill.scala
 *  (FScape)
 *
 *  Copyright (c) 2001-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.fscape
package stream

import akka.stream.stage.InHandler
import akka.stream.{Attributes, SinkShape}
import de.sciss.fscape.stream.impl.{NodeImpl, StageImpl}

import scala.concurrent.Promise

object Fulfill {
  def apply(in: OutD, p: Promise[Double])(implicit b: Builder): Unit = {
    val stage0  = new Stage(p)
    val stage   = b.add(stage0)
    b.connect(in, stage.in)
  }

  private final val name = "Fulfill"

  private type Shape = SinkShape[BufD]

  private final class Stage(p: Promise[Double])(implicit ctrl: Control) extends StageImpl[Shape](name) {
    val shape = new SinkShape(
      in = InA (s"$name.in")
    )

    def createLogic(attr: Attributes) = new Logic(shape, p)
  }

  private final class Logic(shape: Shape, p: Promise[Double])(implicit ctrl: Control)
    extends NodeImpl(name, shape) with InHandler {

    setHandler(shape.in , this)

    override def preStart(): Unit =
      pull(shape.in)

    // ---- InHandler ----

    def onPush(): Unit = {
      val buf   = grab(shape.in)
      val value = buf.buf(0)
      buf.release()
      p.trySuccess(value)
      completeStage()
    }

    override def onUpstreamFinish(): Unit = {
      p.tryFailure(new Exception("Closed before value was available"))
      super.onUpstreamFinish()
    }

    override def onUpstreamFailure(ex: Throwable): Unit = {
      p.tryFailure(ex)
      super.onUpstreamFailure(ex)
    }
  }
}