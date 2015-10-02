package peacock.core

import atk.util.ColorTools

trait CoreConfig {
  val colorMap = Map("LIN-1" -> "0xff99cc",
    "LIN-2" -> "0x0000ff", "LIN-3" -> "0x500079", "LIN-4" -> "0xff0000", "LIN-5" -> "0x994c00", "LIN-6" -> "0x006600", "LIN-7" -> "0xffff00", "LIN-animal" -> "0x00ff9c", "LIN-B" -> "0xff8000", "M-CANETTII" -> "0x00ffff").mapValues(ColorTools.decodeColor(_))
    
}