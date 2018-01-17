package peacock.vignets;

import peacock.core.VignetMaker
import atk.util.ColorGradient
import processing.core.PGraphics
import processing.core.PApplet
import processing.core.PImage
import java.awt.Color
import processing.core.PConstants
import peacock.support.PTools

class HeatMapVignet(header: List[String], valueMapping: Map[String, List[Double]]) extends VignetMaker {

  val gradient = ColorGradient.whiteBlackGradient

  override def y() = { 16 }
  override def x() = { 16 * (valueMapping.map(_._2.size).max) }
  override def headerHeight = {

    if (header.size == 0) 50 else
      (header.map(f => PTools.textWidth(f))).max.toInt

  }
  override def header(buf: PGraphics) {
    assume(x > 0, "X is zero")
    assume(headerHeight > 0, "headerHeight is zero")
    buf.pushMatrix()
    buf.fill(0)
    buf.noStroke()
    buf.translate(0, headerHeight)

    buf.rotate(-PConstants.HALF_PI)
    for (h <- header) {

      buf.text(h, 0, 12);
      buf.translate(0, 16)
    }

    buf.popMatrix()
    buf.stroke(0)
  }

  override def image(buf: PGraphics, key: String) {
    buf.pushMatrix()

    val values = valueMapping.getOrElse(key, List.empty[Double]).zipWithIndex

    for ((v, idx) <- values) {
      val color = gradient.getColor(v)

      buf.fill(buf.color(color.getRed(), color.getGreen(), color.getBlue()))
      buf.rect(1, 1, x - 3, y - 3)
      buf.translate(16, 0)
    }
    buf.popMatrix()

  }

}