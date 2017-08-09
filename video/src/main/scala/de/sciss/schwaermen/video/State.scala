package de.sciss.schwaermen.video

object State {
  trait Text  extends State
  trait Trunk extends State
}
sealed trait State {
  def init(): Unit
}