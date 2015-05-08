package peacock.support

import processing.core.PApplet
import processing.core.PConstants

object PTools {

  private lazy val applet = new PApplet
  applet.init()
  private lazy val graphics = applet.createGraphics(10, 10, PConstants.JAVA2D);
  graphics.colorMode(PConstants.RGB, 255)
  def textWidth(str: String) = {
    graphics.textWidth(str)
  }

  def color(c: Int) = {
    graphics.color(c)
  }

  def color(r: Int, g: Int, b: Int) = {

    graphics.color(r, g, b)
  }

  def dispose() {
    applet.dispose()
  }

}