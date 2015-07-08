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

class CategoryVignets(val dataMap: Map[String,String], val categoryCoding: File, val width:Int=12,val height:Int=12) extends VignetMaker with Lines {

 

  override def toString() ={
    "CategoryVignet:\n\tcategory="+dataMap.take(2).toString+"\n\tcoding="+categoryCoding
  }
  
  val colorMap = if (categoryCoding != null) {
    assume(categoryCoding.exists())
    tMap(tLines(categoryCoding)).mapValues(f => {
//      val arr = f.split("\t")
      ColorTools.decodeColor(f)
//      assume(arr.size==3,"Incorrect number of values for RGB: "+arr.toList)
//      new Color(arr(0).toInt, arr(1).toInt, arr(2).toInt)
    })
  } else {
    val list = dataMap.values.toSet.toList
    (for (key <- list.zipWithIndex) yield {
      key._1 -> ColorPalette(key._2)
    }).toMap

  }
  val defaultColor=colorMap.getOrElse("default", Color.LIGHT_GRAY)
  val missingColor=colorMap.getOrElse("missing", Color.WHITE)

  override def y() = { height }
  override def x() = { width+2 /*+ (colorMap.keys.toList.map(f => PTools.textWidth(f)).max).toInt */}
  override def headerHeight = {
    14 * colorMap.size +20
  }
  override def header(buf: PGraphics) {
    buf.pushMatrix()
    buf.translate(0,20)
    for (pk <- colorMap) {
      val c = pk._2
      buf.fill(buf.color(c.getRed(), c.getGreen(), c.getBlue()))

      buf.rect(0, 0, width-1, height-1)

      buf.text(pk._1, width+2, height-2)
      buf.translate(0, height+2)
    }
    buf.popMatrix()
  }

  override def image(buf: PGraphics, key: String) {
    buf.pushMatrix
    buf.fill(0)
    buf.stroke(255)
    //    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    if (!dataMap.contains(key))
      println("Missing key: " + key)

    val l = dataMap.getOrElse(key, "missing")

    val c = if(!dataMap.contains(key))missingColor else colorMap.getOrElse(l, defaultColor)
    buf.fill(buf.color(c.getRed(), c.getGreen(), c.getBlue()))

    buf.rect(0, 0, width-1, height-1)

    buf.popMatrix()
  }

}