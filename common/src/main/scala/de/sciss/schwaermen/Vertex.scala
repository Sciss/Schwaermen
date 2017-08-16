/*
 *  Vertex.scala
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

import java.io.DataInputStream

import de.sciss.span.Span

import scala.util.control.NonFatal

object Vertex {
  final val WORD_COOKIE = 0x576f7264  // "Word"

  final case class Word(index: Int, span: Span, text: String, fadeIn: Int = 0, fadeOut: Int = 0) {
    override def toString: String = {
      val fadeInS  = if (fadeIn  == 0) "" else s", fadeIn = $fadeIn"
      val fadeOutS = if (fadeOut == 0) "" else s", fadeOut = $fadeOut"
      s"$productPrefix($index, $span, ${quote(text)}$fadeInS$fadeOutS)"
    }
  }

  def tryReadVertices(textId: Int): Array[Vertex] =
    try {
      readVertices(textId)
    } catch {
      case NonFatal(ex) =>
        Console.err.println(s"Error reading vertex resource ($textId) - ${ex.getClass.getSimpleName} - ${ex.getMessage}")
        null
    }

  def readVertices(textId: Int): Array[Vertex] = {
    val fin = getClass.getResourceAsStream(s"text${textId + 1}words.bin")
    try {
      val din     = new DataInputStream(fin)
      val cookie  = din.readInt()
      require(cookie == WORD_COOKIE)
      val numVertices = din.readShort()
      Array.fill(numVertices) {
        val numWords = din.readShort()
        val words = List.fill(numWords) {
          val index     = din.readShort
          val spanStart = din.readLong
          val spanStop  = din.readLong
          val text      = din.readUTF
          val fadeIn    = din.readByte
          val fadeOut   = din.readByte
          Word(index = index, span = Span(spanStart, spanStop), text = text, fadeIn = fadeIn, fadeOut = fadeOut)
        }
        Vertex(textIdx = textId, words = words)
      }

    } finally {
      fin.close()
    }
  }


  def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt) else String.valueOf(ch)
  }

  /** Escapes characters such as newlines and quotation marks in a string. */
  def escape(s: String): String = s.flatMap(escapedChar)
  /** Escapes characters such as newlines in a string, and adds quotation marks around it.
    * That is, formats the string as a string literal in a source code.
    */
  def quote (s: String): String = "\"" + escape(s) + "\""
}
final case class Vertex(textIdx: Int, words: List[Vertex.Word]) {
  val span        : Span    = Span(words.head.span.start, words.last.span.stop)

  val index       : Int     = words.head.index
  val lastIndex   : Int     = words.last.index

  val fadeIn      : Int     = words.head.fadeIn
  val fadeOut     : Int     = words.last.fadeOut

  def wordsString : String  = words.map(_.text).mkString(" ")

  def quote: String =
    Vertex.quote(words.map(_.text).mkString(s"$textIdx-$index: ", " ", ""))

  val duration: Float =
    span.length / 44100f

  val netDuration: Float = duration - (fadeIn + fadeOut) * 0.5f
}