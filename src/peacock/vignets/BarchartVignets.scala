package peacock.vignets

import peacock.core.VignetMaker
import processing.core.PApplet
import processing.core.PImage
import java.awt.Color
import processing.core.PGraphics
import processing.opengl.PGraphics2D

class BarchartVignets(data: Map[String, Map[String, Int]], colors: Map[String, Color], title: String = "", relative: Boolean = false) extends VignetMaker {

  override def y() = { 12 }
  override def x() = { 400 } //(for(m<-data)yield m._2.size).toList.max

  private val effectiveX = x() - 5
  //  }
  override def headerHeight = {
    35 + (if (title.length() > 0) 15 else 0) + colors.size * 15
  }

  override def header(buf: PGraphics) {
    //    val buf: PGraphics = applet.createGraphics(x, headerHeight); //createGraphics(890, yPixels);
    buf.pushMatrix()
    //    buf.beginDraw
    buf.stroke(0)
    buf.fill(0)
    if (title.length() > 0) {
      buf.text(title, 0, 12)
      buf.translate(0, 15)
    }

    /*
     * Legend
     */
    buf.text("Legend: ", 0, 12)
    buf.translate(50, 0)
    for ((key, color) <- colors) {
      buf.stroke(buf.color(color.getRGB()))
      buf.fill(buf.color(color.getRGB()))
      buf.rect(0, 5, 10, 10)
      buf.fill(buf.color(0))
      buf.text(key, 15, 12)
      //      tmpX += textWidth(key).toInt + 25
      buf.translate(0, 15)
    }
    buf.translate(-50, 0)

    buf.fill(buf.color(0))
    buf.stroke(buf.color(0))

    buf.line(0, 23, effectiveX, 23)
    for (i <- 0 to 4) {

      val t = if (relative) {
        "" + i * 25 + "%"
      } else {
        "" + (i * 0.25 * max).toInt
      }
      val tw = buf.textWidth(t)
      buf.line(i * (effectiveX / 4.0f), 5, i * (effectiveX / 4.0f), 23)
      buf.text(t, i * (effectiveX / 4.0f) - tw, 17)
    }
    //    buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height);
    buf.popMatrix()
  }
  private val rowSums = data.map(p => p._1 -> (p._2).map(_._2).sum).toMap
  private val max: Double = rowSums.map(_._2).max
  private val barRatio = effectiveX / max

  override def image(buf: PGraphics, key: String) {
    buf.pushMatrix()
     if (data.contains(key)) {
      println("imaging: " + key)
      val list = data.getOrElse(key, null)
      var trackX = 0.0
      for ((variation, count) <- list) {
        val c = colors.getOrElse(variation, null)
        assume(c != null, "Name=" + variation)
        println("\t" + variation + "\t" + count)
        buf.fill(c.getRed, c.getGreen, c.getBlue())
        buf.stroke(c.getRed, c.getGreen, c.getBlue())
        val xSpace =
          if (relative) {
            val localRatio: Float = effectiveX / rowSums.getOrElse(key, 0)
            count * localRatio
          } else
            count * barRatio
        buf.rect(trackX.toFloat, 1, xSpace.toFloat + 1, 10);
        trackX += xSpace

      }
    } else println("missing: " + key)

    
    buf.popMatrix()
  }



}