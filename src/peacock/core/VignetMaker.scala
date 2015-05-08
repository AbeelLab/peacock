package peacock.core

import processing.core.PGraphics
import processing.core.PApplet
import processing.core.PImage

class VignetMaker() {

  /* Height of vignet */
  def y() = { 12 }
  /* Width of vignet */
  def x() = { 1 }

  def headerHeight = 0

  /* Draw stuff at the top  for this vignet */
  def header(buf: PGraphics) = {
	  /* By default, do nothing */
  }

  def image(buf: PGraphics, key: String) {
  }

  def footerHeight = 0
  
  /* Draw stuff at the bottom of this vignet, typically legend */
  def footer(buf: PGraphics) = {
	  /* By default, do nothing */
  }
  

}