package peacock.core

import atk.util.ColorTools

trait CoreConfig {
  val colorMap = Map("LIN-1" -> "0xed00c3",
    "LIN-2" -> "0x0000ff", "LIN-3" -> "0x500079", "LIN-4" -> "0xff0000", "LIN-5" -> "0x4e2c00", "LIN-6" -> "0x006600", "LIN-7" -> "0xff7e00", "LIN-animal" -> "0x00ff9c", "LIN-B" -> "0x00ff9c", "M-CANETTII" -> "0x00ffff").mapValues(ColorTools.decodeColor(_))
    
}