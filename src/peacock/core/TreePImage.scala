package peacock.core

import processing.core.PImage
import processing.core.PApplet
import processing.core.PGraphics
import scala.collection.JavaConversions._
import atk.compbio.tree.Tree
import atk.compbio.tree.TreeNode
import java.io.File
import atk.util.ColorTools
import atk.util.Lines
import atk.util.Tool

class TreePImage(tree: Tree, val treeWidth: Int, labels: List[LabelGenerator], inputVignets: List[VignetMaker], highlights: List[String] = List.empty[String], val lineage: Map[String, String] = Map.empty[String, String], val clusters: Map[String, List[String]] = Map.empty[String, List[String]], val clusterColoring: Map[String, String] = Map.empty[String, String], internalLabels: File) extends PRender with CoreConfig with Tool {

  val internals = if (internalLabels != null) {
    tMap(tLines(internalLabels)).map(p => p._1.split(";").toList.sortBy(identity).mkString(";") -> p._2)
  } else {
    Map.empty[String, String]
  }
  println("Internal labels:\n\t" + internals.mkString("\n\t"))

  val vignets = if (inputVignets.size == 0) List(new VignetMaker) else inputVignets

  val totalChildren = tree.getLeaves(tree.root).size

  println("Children: " + totalChildren)
  val maxH = vignets.map(f => f.y).max

  println("maxH: " + maxH)
  val clusterWidth = if (clusters.isEmpty) 0 else clusters.map(_._2.size).max * (treeWidth * 0.01).toInt + 4
  val totalWidth = vignets.map(f => f.x).sum + treeWidth + clusterWidth + 5
  val maxHeader = vignets.map(f => f.headerHeight).max
  val maxFooter = vignets.map(f => f.footerHeight).max
  val totalHeight = maxH * totalChildren + maxHeader + maxFooter + 12
  val clusterColorMap = clusterColoring.mapValues(ColorTools.decodeColor(_))
  println("TW = " + totalWidth)

  override def render(buf: PGraphics) {
    render(buf, false)
  }

  def render(buf: PGraphics, mirrorText: Boolean) {
    buf.pushMatrix()
    val maxTextWidth = tree.getLeaves(tree.root).map(p => {
      labels.map(labelGen =>
        buf.textWidth(labelGen.label(p.getName()))).sum
    }).max

    val xMultiplier = (treeWidth - 10 - maxTextWidth) / tree.root.longestWeight //tree.getHeight()

    println("Treeheight=" + tree.getHeight())
    println("TreeiLenght=" + tree.root.longestWeight)

    buf.pushMatrix()
    buf.translate(0, maxHeader)
    println("max text width=" + maxTextWidth)
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
    /*
   * Header
   */
    buf.pushMatrix()
    for (vm <- vignets) {
      buf.pushMatrix()
      buf.translate(treeWidth, 0)
      val img = vm.header(buf)
      //      buf.image(img, treeWidth, 0)
      buf.popMatrix()
      buf.translate(vm.x, 0)
    }
    buf.popMatrix()

    buf.pushMatrix()
    buf.translate(0, maxHeader)

    /**
     * Returns center Y of subtrees
     */
    def drawRecursive(node: TreeNode, yOffset: Int, xOffset: Double): Int = {
      val x = xOffset //(node.height) * chunkX

      val leafySet = tree.getLeaves(node).map(_.getName())
      val llls = leafySet.filter(lineage.contains(_)).map(f => lineage(f)).toSet

      def setColor() {
        if (llls.size == 1 && colorMap.contains(llls.head)) {
          val c = colorMap(llls.head)
          buf.fill(c.getRed(), c.getGreen(), c.getBlue())
          buf.stroke(c.getRed(), c.getGreen(), c.getBlue())
        } else {
          buf.fill(0)
          buf.stroke(0)
        }
      }

      def drawClusters() {
        if (!clusters.isEmpty) {
          val clusterLevels = clusters(node.getName)
          var x = treeWidth + 5
          def recursiveClusters(ls: List[String]): Unit = ls match {
            case head :: tail => {
              val c = clusterColorMap(head)
              buf.fill(c.getRed, c.getGreen, c.getBlue)
              buf.stroke(c.getRed, c.getGreen, c.getBlue)
              buf.rect(x, yOffset + 2, (treeWidth * 0.008).toInt, 12) // Colored bar indicating cluster
              x = x + (treeWidth * 0.01).toInt
              recursiveClusters(tail)
            }
            case Nil =>
          }
          recursiveClusters(clusterLevels)
        }
      }

      setColor // Removing this line does not change anything?

      val cc = node.children
      if (cc.size() == 0) { //If leave node

        buf.line(x.toInt, yOffset + maxH / 2, (x + node.weight * xMultiplier).toInt, yOffset + maxH / 2)
        buf.stroke(buf.color(190, 190, 190))
        buf.line((x + node.weight * xMultiplier).toInt + 2, yOffset + maxH / 2, treeWidth - maxTextWidth - 5, yOffset + maxH / 2)
        //        buf.stroke(buf.color(0, 0, 0))
        buf.stroke(buf.color(0))

        buf.pushMatrix()
        //setColor 
        labels.map(labelGen => {
          val txt = labelGen.label(node.getName())
          buf.pushMatrix()
          drawClusters
          buf.translate(treeWidth - maxTextWidth, yOffset + maxH / 2)
          if (highlights.contains(node.getName())) {
            buf.fill(buf.color(255, 153, 153))
            buf.noStroke()
            buf.rect(0, -maxH / 2 + 2, maxTextWidth, maxH)
            setColor
          }

          if (mirrorText)
            buf.scale(1, -1)
          setColor //Sets lineage colors of txt
          buf.text(txt, 0, (buf.textSize) / 2)
          buf.popMatrix()
          buf.translate(buf.textWidth(txt) + 2, 0)
        })
        buf.popMatrix()

        buf.pushMatrix()
        for (vm <- vignets) {

          buf.pushMatrix()
          //          val img = vm.image(applet, node.getName)
          buf.translate(treeWidth, yOffset + 2)
          println("drawing: " + node.getName() + " " + treeWidth + ", " + yOffset + 2)
          vm.image(buf, node.getName)
          //          buf.image(img, treeWidth, yOffset + 2)
          buf.popMatrix()

          buf.translate(vm.x, 0)
        }
        buf.popMatrix()

        yOffset + maxH / 2

      } else { //if not a leave node

        val childrens = cc.map(tree.getLeaves(_).size)

        val scanResult = cc.scanLeft(0)((acc, child) => {
          acc + tree.getLeaves(child).size
        })

        val zipper = scanResult.zip(childrens)

        def calculateY(zipperPair: Tuple2[Int, Int]) = {
          (zipperPair._2 * maxH) / 2 + zipperPair._1 * maxH;
        }

        val subtrees = cc.zip(zipper);

        val yPositions = subtrees.map(f =>
          drawRecursive(f._1, f._2._1 * maxH + yOffset, x + node.weight * xMultiplier))

        val topY = yPositions.min //calculateY(zipper.first)
        val bottomY = yPositions.max //calculateY(zipper.last)

        val y = (yPositions.min + yPositions.max) / 2
        setColor
        buf.line(x.toInt, y, (x + node.weight * xMultiplier).toInt, y)
        buf.line((x + node.weight * xMultiplier).toInt, topY, (x + node.weight * xMultiplier).toInt, bottomY);

        val internalID = leafySet.toList.sortBy(identity).mkString(";")

        println("Internal node id: " + internalID)
        val internalLabel = internals.getOrElse(internalID, "")
        if(internalLabel.size>0){
          internalLabel.split("\t").zipWithIndex.map{t=>
          buf.text(t._1, (x + node.weight * xMultiplier).toInt,y+t._2*12)}
        }
        

        y
      }

    }
    val y = drawRecursive(tree.root, 0, 5);

    buf.popMatrix() //complete center

    /**
     * Footer
     */
    buf.pushMatrix()
    buf.translate(0, totalHeight - maxFooter)

    for (vm <- vignets) {
      buf.pushMatrix()
      buf.translate(treeWidth, 0)
      val img = vm.footer(buf)
      buf.popMatrix()
      buf.translate(vm.x, 0)
    }

    buf.popMatrix() //complete footer

    buf.popMatrix() //complete draw
  }

}