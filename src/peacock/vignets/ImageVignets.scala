package peacock.vignets

import peacock.core.VignetMaker
import java.io.File
import processing.core.PApplet
import processing.core.PImage
import processing.core.PGraphics

class ImageVignets(fileMapping: Map[String, File], width: Int, height: Int) extends VignetMaker {
  override def y() = { height }
  override def x() = { width}

  override def image(buf:PGraphics, key: String) {
//    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    
    val img = fileMapping.getOrElse(key, null);

    buf.pushMatrix()
//    buf.beginDraw
    if (img != null){
      val ap=new PApplet
      buf.image(ap.loadImage(img.toString()), 0, 0)
    }else{
      println("missing picture: "+key)
    }
    buf.popMatrix()
//    buf.endDraw()
//    buf.get(0, 0, buf.width, buf.height);

  }
}
