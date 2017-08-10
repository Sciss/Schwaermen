package de.sciss.schwaermen.video

import scala.concurrent.stm.InTxn

object Scene {
  trait Text  extends Scene
  trait Trunk extends Scene
}
sealed trait Scene {
  def init()(implicit tx: InTxn): Unit
}