package peacock.vignets

import peacock.core.DualVignetMaker
import java.io.File
import processing.core.PGraphics
import atk.util.Lines
import atk.util.ColorGradient
import java.awt.Color

class DualHeatMap(matrix: File,gradientSize:Int) extends DualVignetMaker with Lines {

  val lines = tLines(matrix)

  val header = lines.take(1)(0).split("\t").drop(1)

  var max = 0
  val triplets = lines.drop(1).map(line => {
    val arr = line.split("\t")
    val pairs = header.zip(arr.drop(1)).toList.toMap.mapValues(_.toInt)
    val mm = pairs.values.toList.max
    if (mm > max)
      max = mm
    arr(0) -> pairs

  }).toMap

  if(gradientSize>0)
    max=gradientSize-1
  
  private val gradient = new ColorGradient(max + 1, Color.WHITE, Color.BLACK)

  override def legend(buf: PGraphics) {
    buf.pushMatrix()

    buf.noStroke()
    buf.text("0", 50-buf.textWidth("0")/2, 77)
    buf.text(max + 1, 250-buf.textWidth(""+(max+1))/2, 77)

    for (i <- 0 until 100) {
      val d = i / 100.0
      val c = gradient.getColor(d)
      buf.fill(buf.color(c.getRed(), c.getBlue(), c.getGreen()))
      buf.rect(50, 50, 2, 15)
      buf.translate(2, 0)

    }

    buf.popMatrix()
    buf.noFill()
    buf.stroke(buf.color(120, 120, 120))
    buf.rect(50, 50, 200, 15)

  }

  override def image(buf: PGraphics, xKey: String, yKey: String) {
    val snpCount = triplets.getOrElse(xKey, null).getOrElse(yKey, -1)
    val c = gradient.getColor(snpCount)
    buf.noStroke()
    println(xKey + "\t" + yKey + "\t" + snpCount + "\t" + List(c.getRed(), c.getBlue(), c.getGreen()).mkString(","))
    //    buf.stroke(buf.color(c.getRed(), c.getBlue(), c.getGreen()))
    buf.fill(buf.color(c.getRed(), c.getBlue(), c.getGreen()))

    buf.rect(0, 0, 12, 12)

    //    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    //    buf.beginDraw
    //    buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height);

  }
}