package peacock.vignets

import processing.core.PApplet
import processing.core.PImage
import processing.core.PGraphics
import peacock.core.VignetMaker

class CoordinateLineVignets(map: Map[String, List[(Int, Int)]], genomeSize: Int) extends VignetMaker {
  override def x() = {
    1200
  }
  override def y() = {
    15
  }

  private def rescale(t: Int) = {
    (t / genomeSize.toFloat) * x
  }

  override def image(buf: PGraphics, key: String) {
//    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
//    buf.beginDraw
    buf.pushMatrix()
    buf.stroke(0)
    buf.fill(0)
    if (map.contains(key)) {
      for (pair <- map.getOrElse(key, List.empty[(Int, Int)])) {
        val x1 = rescale(pair._1)
        val x2 = rescale(pair._2)
        println(x1 + "\t" + x2)
        buf.rect(x1, 2, x2 - x1 + 1, 10)
      }

    } else {
      println("Missing: " + key)
    }

//    buf.endDraw()
//    buf.get(0, 0, buf.width, buf.height);
    buf.popMatrix()

  }

}