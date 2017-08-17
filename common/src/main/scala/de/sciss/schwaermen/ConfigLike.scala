package de.sciss.schwaermen

import java.net.InetSocketAddress

trait ConfigLike {
  def isLaptop            : Boolean
  def dumpOSC             : Boolean
  def disableEnergySaving : Boolean
  def ownSocket           : Option[InetSocketAddress]
  def dot                 : Int
  def log                 : Boolean
}
