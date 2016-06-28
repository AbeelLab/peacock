package peacock.core

import processing.core.PImage
import processing.core.PApplet
import processing.core.PGraphics
import scala.collection.JavaConversions._
import atk.compbio.tree.Tree
import atk.compbio.tree.TreeNode

class RadialPImage(tree: Tree, val canvasSize: Int, labels: List[LabelGenerator], inputVignets: List[VignetMaker], highlights: List[String] = List.empty[String],lineage:Map[String,String]=Map.empty[String,String]) extends PRender with CoreConfig {

  val vignets = if (inputVignets.size == 0) List(new VignetMaker) else inputVignets

  val totalChildren = tree.getLeaves(tree.root).size

  val totalWidth = canvasSize
  val totalHeight = canvasSize
  val labelLen = 0 /* Set to value > 0 to display labels */
  val vignetLen = vignets.map(f => f.x).sum

  println("Vignetlens: " + vignets.map(_.x).mkString("\t"))

  println("Children: " + totalChildren)
 

  override def render(buf: PGraphics) {
    render(buf, false)
  }

  def render(buf: PGraphics, mirrorText: Boolean) {
    buf.pushMatrix()
  

    val treeRadius = (canvasSize - 2 * vignetLen - 2 * labelLen) / 2
    val xMultiplier = (treeRadius - 10) / tree.root.longestWeight //tree.getHeight()
    val radianIncrement = (2 * math.Pi - 0.05) / totalChildren
    println("Treeheight=" + tree.getHeight())
    println("TreeiLenght=" + tree.root.longestWeight)
    println("Tree radius=" + treeRadius)

    /**
     * Legend
     */
    buf.pushMatrix()

    //    println("max text width=" + maxTextWidth)
    val legendVal = 0.02
    val legendX = (20 + legendVal * xMultiplier).toInt
    buf.line(20, 20, legendX, 20)
    buf.line(20, 16, 20, 24)
    buf.line(legendX, 16, legendX, 24)
    val tw1 = buf.textWidth("0")
    val tw2 = buf.textWidth("" + legendVal)
    buf.text("0", 20 - tw1 / 2, 15)
    buf.text("" + legendVal, legendX - tw2 / 2, 15)
    buf.popMatrix()
    buf.translate(canvasSize / 2, canvasSize / 2)

    /**
     * Center
     */

    /**
     * Returns center angle of subtree
     */
    def drawRecursive(node: TreeNode, angleOffset: Double, zOffset: Double): Double = {
      val llls = tree.getLeaves(node).map(_.getName()).filter(lineage.contains(_)).map(f => lineage(f)).toSet

      def setColor() {
        if (llls.size == 1 && colorMap.contains(llls.head)) {
          val c = colorMap(llls.head)
          buf.fill(c.getRed(), c.getGreen(), c.getBlue())
          buf.stroke(c.getRed(), c.getGreen(), c.getBlue())
        } else {
          buf.fill(40)
          buf.stroke(40)
        }
      }
      
      setColor

      val cc = node.children
      /**
       * Leaf node
       */
      if (cc.size() == 0) {

        val z1 = zOffset
        val z2 = zOffset + node.weight * xMultiplier
        val sint = math.sin(angleOffset + radianIncrement)
        val cost = math.cos(angleOffset + radianIncrement)
        buf.strokeWeight(5)
        buf.line((z1 * cost).toFloat, (z1 * sint).toFloat, (z2 * cost).toFloat, (z2 * sint).toFloat)

     

        buf.stroke(buf.color(190, 190, 190))
        buf.strokeWeight(1)
        val z3 = treeRadius
        buf.line((z2 * cost).toFloat, (z2 * sint).toFloat, (z3 * cost).toFloat, (z3 * sint).toFloat)
       
        buf.pushMatrix()
       setColor
        buf.strokeWeight(5)
        buf.translate((z3 * cost).toFloat, (z3 * sint).toFloat)
        buf.rotate(angleOffset.toFloat)
        labels.map(labelGen => {
          val txt = labelGen.label(node.getName())

          buf.pushMatrix()

        

          if (mirrorText)
            buf.scale(1, -1)
          if (labelLen > 0)
            buf.text(txt, 0, (buf.textSize) / 2)
          buf.popMatrix()
          buf.translate(buf.textWidth(txt) + 2, 0)
        })
        buf.popMatrix()

        buf.pushMatrix()
        buf.translate((z3 * cost).toFloat, (z3 * sint).toFloat)
        buf.rotate(angleOffset.toFloat)
        buf.translate(labelLen, 0)
        for (vm <- vignets) {

          buf.pushMatrix()
          //          val img = vm.image(applet, node.getName)
          buf.translate(0, -vm.y / 2)
          //          println("drawing: " + node.getName() + " " + treeWidth + ", " + yOffset + 2)
          vm.image(buf, node.getName)
          //          buf.image(img, treeWidth, yOffset + 2)
          buf.popMatrix()

          buf.translate(vm.x, 0)
        }
        buf.popMatrix()

        angleOffset + radianIncrement
        //        yOffset + maxH / 2
        /**
         * Internal node
         */
      } else {

        // How many grandchildren does each child have
        val grandchildren = cc.map(tree.getLeaves(_).size)

        val cumulativeGrandchildren = cc.scanLeft(0)((acc, child) => {
          acc + tree.getLeaves(child).size
        })

        val zipper = cumulativeGrandchildren.zip(grandchildren)

        def calculateAngle(zipperPair: Tuple2[Int, Int]) = {

          (zipperPair._2 * radianIncrement) / 2 + (zipperPair._1) * radianIncrement;
        }

        val subtrees = cc.zip(zipper);

        val anglePositions = subtrees.map(f =>
          drawRecursive(f._1, f._2._1 * radianIncrement + angleOffset, zOffset + node.weight * xMultiplier))

        val topAngle = anglePositions.min //calculateY(zipper.first)
        val bottomAngle = anglePositions.max //calculateY(zipper.last)

        val middleAngle = (anglePositions.min + anglePositions.max) / 2
      setColor

        val z1 = zOffset
        val z2 = zOffset + node.weight * xMultiplier
        val sint = math.sin(middleAngle)
        val cost = math.cos(middleAngle)

        println("Arc: " + z2 + "\t" + bottomAngle + "\t" + topAngle)
        buf.noFill

        buf.arc(0, 0, 2 * z2.toFloat, 2 * z2.toFloat, topAngle.toFloat, bottomAngle.toFloat)

        buf.line((z1 * cost).toFloat, (z1 * sint).toFloat, (z2 * cost).toFloat, (z2 * sint).toFloat)

        //        buf.line((z2 * math.cos(bottomAngle)).toFloat, (z2 * math.sin(bottomAngle)).toFloat, (z2 * math.cos(topAngle)).toFloat, (z2 * math.sin(topAngle)).toFloat)
        //        buf.line(x.toInt, y, (x + node.weight * xMultiplier).toInt, y)

        //        buf.line((x + node.weight * xMultiplier).toInt, topY, (x + node.weight * xMultiplier).toInt, bottomY);

        //        y
        middleAngle
      }

    }
    val y = drawRecursive(tree.root, 0, 0);

    buf.popMatrix()
  }

}