package de.sciss.schwaermen
package video

import java.net.SocketAddress

import de.sciss.osc

import scala.annotation.switch
import scala.concurrent.stm.{InTxn, Ref}

object Scene {

  trait Text  extends Scene
  trait Trunk extends Scene

  val current: Ref[Scene] = Ref.make[Scene]

  object OscInjectQuery {
    private[this] val Name = "/inject-query"

    def apply(uid: Long, videoId: Int, vertex: Int) = osc.Message(Name, uid, videoId, vertex)

    def unapply(m: osc.Message): Option[(Long, Int, Int)] = m match {
      case osc.Message(Name, uid: Long, videoId: Int, vertex: Int) => Some((uid, videoId, vertex))
      case _ => None
    }
  }

  object OscInjectReply {
    object Mode {
      def apply(id: Int): Mode = (id: @switch) match {
        case Ignored  .id => Ignored
        case Accepted .id => Accepted
        case Rejected .id => Rejected
      }
    }
    sealed trait Mode { def id: Int }
    case object Ignored  extends Mode { final val id = 0 }
    case object Accepted extends Mode { final val id = 1 }
    case object Rejected extends Mode { final val id = 2 }

    private[this] val Name = "/inject-reply"

    def apply(uid: Long, m: Mode): osc.Message = osc.Message(Name, uid, m.id)

    def unapply(m: osc.Message): Option[(Long, Mode)] = m match {
      case osc.Message(Name, uid: Long, id: Int) => Some((uid, Mode(id)))
      case _ => None
    }
  }

  object OscInjectAbort {
    private[this] val Name = "/inject-abort"

    def apply(uid: Long) = osc.Message(Name, uid)
    def unapply(m: osc.Message): Option[Long] = m match {
      case osc.Message(Name, uid: Long) => Some(uid)
      case _ => None
    }
  }

  object OscInjectCommit {
    private[this] val Name = "/inject-commit"

    def apply(uid: Long, targetDot: Int) = osc.Message(Name, uid, targetDot)
    def unapply(m: osc.Message): Option[(Long, Int)] = m match {
      case osc.Message(Name, uid: Long, targetDot: Int) => Some((uid, targetDot))
      case _ => None
    }
  }
}
sealed trait Scene {
  def init()(implicit tx: InTxn): Unit

  def queryInjection(sender: SocketAddress, uid: Long, meta: PathFinder.Meta,
                     ejectVideoId: Int, ejectVertex: Int)(implicit tx: InTxn): Unit
}