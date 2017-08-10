package de.sciss.schwaermen
package video

final case class QueryResult[+A](dot: Int, value: A)