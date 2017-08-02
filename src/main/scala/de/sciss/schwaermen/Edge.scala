package de.sciss.schwaermen

trait EdgeLike[+A] {
  def start : A
  def end   : A
  def weight: Double
}

final case class Edge[+A](start: A, end: A, weight: Double) extends EdgeLike[A] {
  override def toString: String = {
    val w1 = s"$weight"
    val w2 = f"$weight%g"
    val w3 = if (w1.length < w2.length) w1 else w2
    s"$productPrefix(start = $start, end = $end, weight = $w3)"
  }
}
