package peacock.core

import processing.core.PImage
import processing.core.PApplet
import processing.core.PGraphics
import scala.collection.JavaConversions._
import atk.compbio.tree.Tree
import atk.compbio.tree.TreeNode

class TablePImage(sorting: List[String], labels: List[LabelGenerator], inputVignets: List[VignetMaker], highlights: List[String] = List.empty[String],applet:PApplet) extends PRender{

  val vignets = if (inputVignets.size == 0) List(new VignetMaker) else inputVignets

  val totalChildren = sorting.size

  println("Children: " + totalChildren)
  val maxH = vignets.map(f => f.y).max

  println("maxH: " + maxH)

   val maxTextWidth = sorting.map(p => {
      labels.map(labelGen =>
        applet.textWidth(labelGen.label(p))).sum
    }).max
  
  val totalWidth = (vignets.map(f => f.x).sum + maxTextWidth + 5).toInt +5
  val maxHeader = vignets.map(f => f.headerHeight).max
  val totalHeight = maxH * totalChildren + maxHeader + 5

  //  println("TW = " + totalWidth)

  override def render(buf: PGraphics) {
    render(buf,false)
  }

  def render(buf: PGraphics, mirrorText: Boolean) {
    buf.pushMatrix()
    buf.translate(5,0)
   

  
    /*
   * Header
   */
    buf.pushMatrix()
    for (vm <- vignets) {
      buf.pushMatrix()
      buf.translate(maxTextWidth, 0)
      val img = vm.header(buf)
      //      buf.image(img, treeWidth, 0)
      buf.popMatrix()
      buf.translate(vm.x, 0)
    }
    buf.popMatrix()
    buf.translate(0, maxHeader)
    /**
     * Center
     */

    
    buf.fill(0)
    buf.stroke(0)
//    val cc = node.children
    //      if (cc.size() == 0) {
    for (g <- sorting) {
      buf.pushMatrix()
      buf.fill(0)
      buf.stroke(0)
      labels.map(labelGen => {
        val txt = labelGen.label(g)
        buf.pushMatrix()
//        buf.translate(0, yOffset + maxH / 2)
        if (highlights.contains(g)) {
          buf.fill(buf.color(255, 153, 153))
          buf.noStroke()
          buf.rect(0, -maxH / 2 + 2, maxTextWidth, maxH)
          buf.fill(0)
          buf.stroke(0)
        }

        if (mirrorText)
          buf.scale(1, -1)
        buf.text(txt, 0,buf.textSize)
        buf.popMatrix()
        buf.translate(buf.textWidth(txt) + 2, 0)
      })
      buf.popMatrix()

      buf.pushMatrix()
      for (vm <- vignets) {

        buf.pushMatrix()
        //          val img = vm.image(applet, node.getName)
        buf.translate(maxTextWidth, 0)
        println("drawing: " + g )
        vm.image(buf, g)
        //          buf.image(img, treeWidth, yOffset + 2)
        buf.popMatrix()

        buf.translate(vm.x, 0)
      }
      buf.popMatrix()
      buf.translate(0,maxH)

    }
   

    buf.popMatrix()
  }

}