package de.sciss.schwaermen

import java.awt.Font

object MyFont {
  private[this] lazy val _initFont: Font = {
    val url = getClass.getResource("/OpenSans-CondLight.ttf")
    require(url != null)
    val is = url.openStream()
    val res = Font.createFont(Font.TRUETYPE_FONT, is)
    is.close()
    res
  }

  private[this] var _condensedFont: Font = _

  def apply(): Font = {
    if (_condensedFont == null) _condensedFont = _initFont
    _condensedFont
  }

  def apply(size: Float): Font = apply().deriveFont(size)
}