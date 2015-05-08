package peacock.vignets

import scala.collection.JavaConversions._
import processing.core._
import java.awt.Color
import java.io.File
import java.io.PrintWriter
import scala.io.Source
import javax.swing.JFrame
import java.awt.BorderLayout
import javax.swing.JScrollPane
import scala.collection.mutable.HashSet
import atk.util.Tool

import peacock.core.TreePImage
import peacock.core.TreeViz
import peacock.core.TreeViz._
import peacock.core.VignetMaker

/**
 * Good place for color schemes
 *
 * http://colorbrewer2.org/
 *
 */
//object LSPsTree {
//
//  def main(str: Array[String]) {
//    val folder = "u:/Users/Alex/zhang/"
//
//    val tree =
//      new Tree(folder + "midpoint.20131106.nwk")
//    //    gCleanTree(tree)
//
//    //    val order = tLines(folder + "clustering.txt")
//
//    TreeViz.make(tree, treeWidth = 800, vignets = List(new LSPVignets(folder + "20131209_AssociationData.txt")), exportPrefix = folder + "peacock.lspTree.")
//
//    //    TreeViz.make(tree);
//  }
//}

class LSPVignets(inputFile: File) extends BinaryVignets(inputFile, {
  val arr = tLines(inputFile).take(1)(0).split("\t").drop(1).sortBy(item => {
    val split = item.split("\\.")
    if(item.size==0)
      0
    else
      split(split.size - 2).toInt
//    assume(split.size>=2,"Not enough slices for item: $"+item+"$")
    

  }).toList
  arr.zip(arr)

}) {

  private val boxHeight = 12;
  private val boxWidth = 12;

  println("Parsing data...")

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

  override def image(buf: PGraphics, key: String) = {
    //    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    //    buf.beginDraw
    //    buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height);
    val width = x();
    //    val buf = applet.createGraphics(labelWidth, y); //createGraphics(890, yPixels);

    //    buf.beginDraw()

    //    buf.fill(0)

    //    val profile = mapping.getOrElse(key, scala.collection.mutable.Set.empty[String])

    buf.pushMatrix
    buf.translate(3,0)
    //    buf.fill(0)
    //          buf.stroke(0)
    //    val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    if (!matrix.contains(key))
      println("Missing key: " + key)

    val l = matrix.getOrElse(key, List("Missing value: " + key))

    val useL = l.zip(indices).sortBy(_._2)
    //    println("useL: " + useL)
    //    println(defaultOrder)
    //    System.exit(0)
    //    buf.beginDraw

    //    buf.fill(buf.color(255, 0, 0))
    //    buf.rect(0, 0, 10, 10)
    //    buf.background(buf.color(255))
    //    println(useL)
    var idx = 0
    for (pair <- useL) {
      val mut = pair._1
      //      val xidx = allDeletionIdentifiers.indexOf(mut)
      //      if (xidx == -1)
      //        println(allDeletionIdentifiers.contains(mut) + "\t" + allDeletionIdentifiers.size + "\t" + allDeletionIdentifiers.take(5))

      //      assume(x != -1, "Wrong Mutation: " + mut + "\t" + x)
      val mutation=useOrder(pair._2)._1
      val split =   if(mutation.size==0)
        "NSP.None.0.0.0".split("\\.")
      else
        mutation.split("\\.")
    
      assume(split.size > 3, "Mutation: " +mutation )

      var alpha = 255
      val size = split(split.size - 1).toInt
      if (mut.equals("1")) {
    	 split(1) match {
          case "Insertion" =>

            buf.stroke(buf.color(0, 114, 178, alpha))
            buf.fill(buf.color(0, 114, 178, alpha))

          case "Deletion" =>
            buf.stroke(buf.color(230, 159, 0, alpha))
            buf.fill(buf.color(230, 159, 0, alpha))
          case "Substitution" =>
            buf.stroke(buf.color(0, 158, 115, alpha))
            buf.fill(buf.color(0, 158, 115, alpha))
          case _ =>
            buf.stroke(buf.color(255, 255, 255, alpha))
            buf.fill(buf.color(255, 255, 255, alpha))
        }
        buf.rect(0, 0, boxWidth - 1, boxHeight - 1)
      }

      buf.translate(boxWidth, 0)
      buf.stroke(buf.color(200, 200, 200, alpha))
        buf.line(0, 0,0,boxWidth - 1)
      
      
      idx += 1
      if (idx % 10 == 0){
         buf.stroke(buf.color(150, 150, 150, alpha))
        buf.line(6, 0, 6,boxWidth - 1)
        buf.translate(12, 0)
      }
    }
    buf.popMatrix()

    //    buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height)

  }

}

