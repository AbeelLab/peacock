package peacock.freeform

import peacock.core.FreeFormAddition
import java.awt.Color
import processing.core.PGraphics
import processing.pdf.PGraphicsPDF

class CategoryRadialLegend(val mapping: Map[String, Color]) extends FreeFormAddition {

  override def drawFreeForm(buf: PGraphics) = {
    println("drawing categoy legend")
    val width=24
    val height=24
    buf.pushMatrix()
  
    
    
    buf.translate(20,PGraphicsPDF.RESCALE_FACTOR*buf.height-(mapping.size+1)*(height+3)+20)
    println(mapping)
    //FIXME only include used colors
    buf.noStroke()
    val tx=buf.textSize
    buf.textSize(height)
    for (pk <- mapping) {
      val c = pk._2
      buf.fill(buf.color(c.getRed(), c.getGreen(), c.getBlue()))

      buf.rect(0, 0, width-1, height-1)

      buf.text(pk._1, width+2, height-2)
      buf.translate(0, height+3)
    }
    buf.textSize(tx)
    buf.popMatrix()
  }

}