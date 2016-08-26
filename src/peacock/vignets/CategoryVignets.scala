package peacock.vignets

import peacock.core.VignetMaker
import atk.util.Lines
import processing.core.PGraphics
import processing.core.PApplet
import processing.core.PImage
import processing.core.PConstants
import java.io.File
import atk.util.ColorPalette
import java.awt.Color
import peacock.support.PTools
import atk.util.ColorTools
import scala.collection.JavaConversions._

object CategoryVignets {
  var usedLegendLines = 0
}

class CategoryVignets(val inputMap: Map[String, String], val colorMap: Map[String, Color], val width: Int = 12, val height: Int = 12, ordering: List[(String, String)] = null) extends VignetMaker with Lines {

  override def toString() = {
    "CategoryVignet:\n\tcategory=" + dataMap.take(2).toString + "\n\tcoding=" + colorMap
  }

  val defaultColor = colorMap.getOrElse("default", Color.LIGHT_GRAY)
  val missingColor = colorMap.getOrElse("missing", Color.WHITE)

  private val dataMap = inputMap.mapValues(_.split("\t"))

  private val tmp = dataMap.getOrElse("$$", null).toList
  println("TMP: " + tmp)
  assert(tmp != null, "The matrix file does not appear to have a heading indicated with $$")
  private val defaultOrder = tmp.zip(tmp)

  val useOrder = if (ordering == null) defaultOrder else ordering

  println(dataMap.take(5))

  override def y() = { height }
  override def x() = { (width + width / 4) * dataMap.head._2.size /*+ (colorMap.keys.toList.map(f => PTools.textWidth(f)).max).toInt */ }
  override def headerHeight = {
    14 * colorMap.size + 20
  }
  override def header(buf: PGraphics) {
    buf.pushMatrix()
    buf.translate(0, 20)
    //FIXME only include used colors
    for (pk <- colorMap) {
      val c = pk._2
      buf.fill(buf.color(c.getRed(), c.getGreen(), c.getBlue()))

      buf.rect(0, 0, width - 1, height - 1)

      buf.text(pk._1, width + 2, height - 2)
      buf.translate(0, height + 2)
    }
    buf.popMatrix()

    /*
     * Draw labels for columns
     */

    buf.pushMatrix()
    buf.fill(0)
    buf.stroke(0)
    buf.translate(0, headerHeight)
    buf.rotate(-PConstants.HALF_PI)
    var idx = 0
    buf.translate(0,-2)
    for (l <- useOrder) {
      buf.text(l._2, 0, 12);
      //buf.translate(0, 12)
      buf.translate(0,width + width / 4)
      idx += 1
//      if (idx % 10 == 0)
//        buf.translate(0, 12)
    }
    buf.popMatrix()
  }

  override def image(buf: PGraphics, key: String) {
    buf.pushMatrix
    buf.fill(0)
    //    buf.stroke(255)
    buf.noStroke
    //    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    if (!dataMap.contains(key))
      println("Missing key: " + key)

    val lx = dataMap.getOrElse(key, List("missing").toArray)

    for (l <- lx.zipWithIndex) {
      val c = if (!dataMap.contains(key)) missingColor else colorMap.getOrElse(l._1, defaultColor)
      buf.fill(buf.color(c.getRed(), c.getGreen(), c.getBlue()))

      buf.rect((width + width / 4) * l._2, 0, width - 1, height - 1)

    }
    buf.stroke(0)
    buf.popMatrix()

  }

}