package peacock.vignets

import peacock.core.VignetMaker
import atk.util.Lines
import processing.core.PGraphics
import processing.core.PApplet
import processing.core.PImage
import processing.core.PConstants
import java.io.File
import atk.util.ColorGradient
import peacock.support.PTools

class BinaryVignets(matrixFile: File,
  ordering: List[(String, String)] = null, colorCoding: File = null) extends VignetMaker with Lines {

  val tmpMatrix = tMap(tLines(matrixFile))
  val matrix = tmpMatrix.mapValues(f => f.split("\t").toList)
  val columns = (matrix.map(_._2.size)).max

  val tmp = matrix.getOrElse("$$", null)
  println("TMP: " + tmp)
  assert(tmp!=null,"The matrix file does not appear to have a heading indicated with $$")
  val defaultOrder = tmp.zip(tmp)

  val useOrder = if (ordering == null) defaultOrder else ordering

  println("useOrder: " + useOrder)
  val indices = defaultOrder.map(f => useOrder.map(_._1).indexOf(f._1))
  println("idic: " + indices)

  
  
  
  override def y() = { 12 }
  override def x() = { math.ceil(columns * 13.2).toInt }

  override def headerHeight = {
    useOrder.map(f => f._2.length() * 7).max
  }

  val labelMap=scala.collection.mutable.Map(0-> "Absent").withDefault(_ =>"Present")
  val colors = scala.collection.mutable.Map(0 -> (PTools.color(255), PTools.color(230))).withDefault(_ => (PTools.color(0), PTools.color(0)))

  if (colorCoding != null) {
    assume(colorCoding.exists(), "Color coding file missing: " + colorCoding)
    val labels=tMap(tLines(colorCoding),1,0,limitSplit=false).map(x=>x._1.toInt->x._2)
    val colorMap = tMap(tLines(colorCoding),1,2).map(_ match {
      case (x: String, y: String) => {
        val colors = y.split("\t").map(c => {
          val rgb = c.split(",").map(_.toInt)
          val color = PTools.color(rgb(0), rgb(1), rgb(2))
          color

        })
        //        
        if(colors.length==1)
        	x.toInt -> (colors(0),colors(0))
        else
        	x.toInt -> (colors(0),colors(1))
      }
      case _ => assert(false); 0 -> (0,0)

    })
    colors ++= colorMap
    labelMap ++= labels
  }

  println(colors)
  
  override def footerHeight()={
    math.max(labelMap.size,2) * 15
  }
  
  override def footer(buf:PGraphics){
    val sorted=labelMap.keys.toList.sortBy(identity)
    for(key<-sorted){
      println("key :"+key+"\t"+colors(key)._1+"\t"+colors(key)._2)
      buf.fill(colors(key)._1)
      buf.stroke(colors(key)._2)
      buf.rect(1,1,12,12)
      buf.fill(0)
      buf.stroke(0)
      buf.text(labelMap(key),15,10)
      buf.translate(0, 15)
    }
  }
  
  
  override def header(buf: PGraphics) {
    assume(x > 0, "X is zero")
    assume(headerHeight > 0, "headerHeight is zero")
    //    val buf: PGraphics = applet.createGraphics(x, headerHeight); //createGraphics(890, yPixels);
    //    buf.beginDraw()
    buf.pushMatrix()
    buf.fill(0)
    buf.stroke(0)
    buf.translate(0, headerHeight)
    buf.rotate(-PConstants.HALF_PI)
    var idx = 0
    for (l <- useOrder) {
      buf.text(l._2, 0, 12);
      buf.translate(0, 12)
      idx += 1
      if (idx % 10 == 0)
        buf.translate(0, 12)
    }
    buf.popMatrix()
    //    buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height);
  }

  override def image(buf: PGraphics, key: String) {
    buf.pushMatrix
    buf.fill(0)
    buf.stroke(0)
    //    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    if (!matrix.contains(key))
      println("Missing key: " + key)

    val l = matrix.getOrElse(key, List("Missing value: " + key))

    val useL = l.zip(indices).sortBy(_._2)
    //    println("useL: " + useL)

    //    buf.beginDraw

    buf.fill(buf.color(255, 0, 0))
    buf.rect(0, 0, 10, 10)
    //    buf.background(buf.color(255))
    //    println(useL)
    var idx = 0
    buf.pushMatrix()
    def parseInt(s: String) = try { Some(s.toInt) } catch { case _ => None }

    for (s <- useL) {

      val ix = parseInt(s._1)
      ix match {
        case Some(i) =>
          println("I: " + i)
          buf.fill(colors(i)._1)
          buf.stroke(colors(i)._2)
          buf.rect(0, 0, 11, 11)
        case _ =>
      }
      /*match {
        case 1 =>
          buf.fill(colors())
          buf.stroke(0)
          buf.rect(0, 0, 11, 11)
        case 0 =>
          buf.fill(255)
          buf.stroke(230)
          buf.rect(0, 0, 11, 11)
        case _ =>
          buf.fill(buf.color(255, 0, 0))
          buf.stroke(0)
      }*/
      idx += 1
      if (idx % 10 == 0) {
        buf.translate(12, 0)
        buf.stroke(buf.color(150, 150, 150, 255))
        buf.line(6, 0, 6, y - 1)

      }
      buf.translate(12, 0)

    }
    buf.popMatrix()

    buf.popMatrix()
  }

}
