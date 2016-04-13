package peacock.vignets

import peacock.core.VignetMaker
import atk.util.Lines
import processing.core.PApplet
import processing.core.PImage
import processing.core.PGraphics
import processing.core.PConstants
import peacock.support.PTools
//val tmpMatrix = tMap(tLines(matrixFile))
class TextVignets(headerLabels: List[String], matrixFile: Map[String, String],highlights:List[String]=List.empty[String],fontSizeMultiplier:Float) extends VignetMaker with Lines {

  
  
  val extraSpacer = 5
  val matrix_in = matrixFile.mapValues(f => f.split("\t").toList)
  val maxLen = matrix_in.map(_._2.size).max
  val matrix = matrix_in.mapValues { f =>
    val l = maxLen - f.size
    f ++ List.fill(l)("")

  }
  //  val defaultOrder = matrix.getOrElse("$$", null)

  override def headerHeight = {
    200
  }
  val columns = if (matrix.isEmpty) 0 else (matrix.map(_._2.size)).max

  override def y() = { 12 }
  override def x() = {
    val sum = columnWidths.sum.toInt
    if(sum>0)
      sum
      else 1
      
    //    matrixFile.values.toList.fla
//    0
    //    if (columns == 0) averageWidth else columns * averageWidth 
  }

  val columnWidths = matrix.filterKeys(!_.contains("$$")).toList.map(_._2).transpose.map(row => {
    row.map(cell => (PTools.textWidth(cell)*fontSizeMultiplier)+extraSpacer).max

  })

  override def header(buf: PGraphics) {

    assume(x > 0, "X is zero")
    assume(headerHeight > 0, "headerHeight is zero")
    //    val buf: PGraphics = applet.createGraphics(x, headerHeight); //createGraphics(890, yPixels);
    buf.pushMatrix()


    buf.fill(0)
    buf.stroke(0)

    buf.translate(-5, headerHeight)

    val zippy = headerLabels.zip(columnWidths)
    for (l <- zippy) {
      buf.rotate(-PConstants.HALF_PI)
      buf.text(l._1, 0, 14);
      buf.rotate(PConstants.HALF_PI)
      buf.translate(l._2 , 0)
    }
    //        buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height);
    buf.popMatrix()
  }

  override def image(buf: PGraphics, key: String) {
    //    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    buf.pushMatrix()
    val fIn=buf.textSize
    buf.textSize(fontSizeMultiplier*fIn)
    val l = matrix.getOrElse(key, List("-"))
    //    buf.beginDraw
    if(highlights.contains(key)){
    	buf.fill(255,0,0)
    	buf.stroke(255,0,0)
    }else{
      buf.fill(0)
      buf.stroke(0)
    }
    	
    //    buf.background(255)
    val zippy = l.zip(columnWidths)
    for (s <- zippy) {
      buf.text(s._1, 0, 10)
      buf.translate(s._2 , 0)
    }
    buf.textSize(fIn)
    //    buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height);

    buf.popMatrix()
  }

}