package peacock.vignets

import peacock.core.VignetMaker

class SpacerVignet(w:Int) extends VignetMaker {
  override def y() = { 1 }
  override def x() = { w }
}