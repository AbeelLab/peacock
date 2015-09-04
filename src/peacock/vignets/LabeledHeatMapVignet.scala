package peacock.vignets;

import peacock.core.VignetMaker
import atk.util.ColorGradient
import processing.core.PGraphics
import processing.core.PApplet
import processing.core.PImage
import java.awt.Color
import processing.core.PConstants


class LabeledHeatMapVignet(header: String, valueLabelList: List[String], valueMapping: Map[String, Int]) extends VignetMaker {

  val gradient = new ColorGradient(valueLabelList.size, Color.blue, Color.green, Color.yellow, Color.red); //new ColorGradient(labelList.size, Color.blue, Color.red)

  override def y() = { 16 }
  override def x() = { 25 }
  override def headerHeight = { 50 }
  override def header(buf: PGraphics){
    assume(x > 0, "X is zero")
    assume(headerHeight > 0, "headerHeight is zero")
//    val buf: PGraphics = applet.createGraphics(x, headerHeight); //createGraphics(890, yPixels);
    buf.pushMatrix()
//    buf.beginDraw()
    buf.fill(0)
    buf.stroke(0)
    buf.translate(0, headerHeight)
    buf.rotate(-PConstants.HALF_PI)

    buf.text(header, 0, 12);

//    buf.endDraw()
//    buf.get(0, 0, buf.width, buf.height);
    buf.popMatrix()
  }

  //  def header(applet: PApplet): PImage = {
  //    val buf: PGraphics = applet.createGraphics(1, 1); //createGraphics(890, yPixels);
  //    buf.beginDraw
  //    buf.endDraw()
  //    buf.get(0, 0, buf.width, buf.height);
  //  }

  override def image(buf: PGraphics, key: String){
    buf.pushMatrix()
//    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
//    buf.beginDraw

    val idx = valueMapping.getOrElse(key, -1)
    println("Getting:" + key + "\t" + idx)
    val color = if (idx >= 0) gradient.getColor(idx) else Color.gray
    val label = if (idx >= 0) valueLabelList(idx) else key

    buf.fill(buf.color(color.getRed(), color.getGreen(), color.getBlue()))
    buf.rect(1, 1, x - 3, y - 3)
    buf.fill(buf.color(0))
    buf.text(label, (x - buf.textWidth(label)) / 2, 12)
//    buf.endDraw()
//    buf.get(0, 0, buf.width, buf.height);
    buf.popMatrix()

  }

}