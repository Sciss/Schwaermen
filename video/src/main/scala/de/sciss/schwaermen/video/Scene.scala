package de.sciss.schwaermen
package video

import java.net.SocketAddress

import de.sciss.osc
import de.sciss.schwaermen.video.Scene.OscReplyInjection

import scala.concurrent.stm.{InTxn, Ref}

object Scene {
//  object Text {
//    sealed trait State
//    case object Idle      extends State
//    case object Querying  extends State
//    case object Ejecting  extends State
//  }
  trait Text extends Scene {
  }

  trait Trunk extends Scene {
  }

  val current: Ref[Scene] = Ref.make[Scene]

  val OscQueryInjection = osc.Message("/query-inject")

  final case class OscReplyInjection(accepted: Boolean)
    extends osc.Message("/reply-inject", accepted)
}
sealed trait Scene {
  def init()(implicit tx: InTxn): Unit

  def queryInjection(sender: SocketAddress)(implicit tx: InTxn): Unit
}