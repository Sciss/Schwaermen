package de.sciss.schwaermen
package video

object State {
  case object Trunk extends State
  case object Text  extends State
}
sealed trait State {

}
