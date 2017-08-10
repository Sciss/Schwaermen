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

  object OscQueryInjection {
    def apply(uid: Long) = osc.Message("/query-inject", uid)

    def unapply(m: osc.Message): Option[Long] = m match {
      case osc.Message("/query-inject", uid: Long) => Some(uid)
      case _ => None
    }
  }

  object OscReplyInjection {
    object Mode {
      def apply(id: Int): Mode = (id: @switch) match {
        case Ignored  .id => Ignored
        case Accepted .id => Accepted
        case Rejected .id => Rejected
      }
    }
    sealed trait Mode { def id: Int }
    case object Ignored  extends Mode { val id = 0 }
    case object Accepted extends Mode { val id = 1 }
    case object Rejected extends Mode { val id = 2 }

    def apply(uid: Long, m: Mode): osc.Message = osc.Message("/reply-inject", uid, m.id)

    def unapply(m: osc.Message): Option[(Long, Mode)] = m match {
      case osc.Message("/reply-inject", uid: Long, id: Int) => Some(uid, Mode(id))
      case _ => None
    }
  }

  object OscAbortTransaction {
    def apply(uid: Long) = osc.Message("/txn-abort", uid)
    def unapply(m: osc.Message): Option[Long] = m match {
      case osc.Message("/txn-abort", uid: Long) => Some(uid)
      case _ => None
    }
  }

  object OscCommitTransaction {
    def apply(uid: Long) = osc.Message("/txn-commit", uid)
    def unapply(m: osc.Message): Option[Long] = m match {
      case osc.Message("/txn-commit", uid: Long) => Some(uid)
      case _ => None
    }
  }
}
sealed trait Scene {
  def init()(implicit tx: InTxn): Unit

  def queryInjection(sender: SocketAddress, uid: Long)(implicit tx: InTxn): Unit
}