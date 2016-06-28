package peacock.support

import scala.collection.JavaConversions._
import java.io.File
import be.abeel.io.PatternFileFilter
import atk.compbio.vcf.VCFFile
import atk.util.Tool
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import atk.util.ColorGradient
import java.awt.Color
import atk.compbio.tree.Tree

/*
 * Program to create expensive viz buffers
 */
object CreateBuffers extends Tool {

  val gradientBlack = new ColorGradient(16, Color.white, Color.black)
  val gradientRed = new ColorGradient(16, Color.white, Color.red)

  //  def main(args: Array[String]): Unit = {
  def execute(workfolder: File, treeFile: File) {
	  assume(workfolder.exists(),"The input directory does not exist, I can't find files in a non-existing directory.")
    def findFile(gNumber: String) = {
      println("Finding: "+gNumber)
      val out = workfolder.listFiles(new PatternFileFilter(gNumber + ".annotated.vcf"))
      assume(out.size == 1)
      out(0)

    }

    val tree = new Tree(treeFile.toString);

    val identifiers = tree.getLeaves(tree.root).map(_.getName())
//    val identifiers = allLeaves.map {
//      case List(x) => x
//      case List() => "reference"
//    }
    for (i <- identifiers.filterNot(p => p.equals("reference"))) {

      //      for (orderPair <- order) {
      //        val outputFile = new File(workfolder + "/" + i + "." + orderPair._1 + ".viz")
      val input = findFile(i)

      //      
      println("Preprocessing : " + i + " - " + input)
      val posFilter = VCFFile(input).map(f => f.pos -> (f.filter, f.refLength, f.altLength)).toList
      val bins = posFilter.groupBy(f => f._1 / 10000)
      val mashed = bins.mapValues(f => f.map(_._2._1).groupBy(identity).mapValues(_.size))
      //      val pw=new PrintWriter(workfolder+"/hawk.preprocess."+i+".txt")
      //      pw.println(generatorInfo)
      //      pw.println("# bin index\tcounts")
      println(mashed)
      val xOffset = 50
      val img = new BufferedImage(950, 30, BufferedImage.TYPE_INT_RGB);

      val g = img.getGraphics();
      g.setColor(Color.white)
      g.fillRect(0, 0, 950, 30)
      mashed.map(pair => {

        val x = pair._1 * 2
        val snpCount = pair._2.getOrElse("PASS", 0)

        val snpColor = if (pair._2.getOrElse("Amb", 0) == 0) {
          gradientBlack.getColor(math.min(snpCount, 15))
        } else {
          gradientRed.getColor(math.min(snpCount, 15))

        }
        g.setColor(snpColor)
        g.fillRect(x + xOffset, 10, 2, 10)

        val covCount = pair._2.getOrElse("LowCov", 0) + pair._2.getOrElse("Amb;LowCov", 0)
        var covColor = gradientBlack.getColor(math.min(covCount, 15))
        g.setColor(covColor)
        g.fillRect(x + xOffset, 20, 2, 10)

      })

      val largeThreshold = 500
      val large = posFilter.filter(triplet => triplet._2._2 > largeThreshold || triplet._2._3 > largeThreshold)
      println(i+" large events: "+large)
      large.map(event => {
        val localX = (event._1 / 10000) * 2 + xOffset

        /* Reference */
        if (event._2._2 > largeThreshold) {
          //          g.fillRect(color(255, 0, 0, 60 * (event.refLength / largeThreshold)))
          g.setColor(new Color(255, 0, 0, math.min(60 * (event._2._2 / largeThreshold), 255)))
          g.setColor(gradientRed.getColor(event._2._2 / largeThreshold))
          val xPoints = List(localX+1, localX + 5, localX - 3)
          val yPoints = List(2, 9, 9)
          g.fillPolygon(xPoints.toArray, yPoints.toArray, 3)
          //          g.drawPolygon(xPoints.toArray, yPoints.toArray, 3)

        }
        /* Alternative */
        if (event._2._3 > largeThreshold) {
          g.setColor(new Color(0, 0, 255, math.min(60 * (event._2._3 / largeThreshold), 255)))

          val xPoints = List(localX - 3, localX + 5, localX+1)
          val yPoints = List(2, 2, 9)
          g.fillPolygon(xPoints.toArray, yPoints.toArray, 3)
          //          g.drawPolygon(xPoints.toArray, yPoints.toArray, 3)
        }
      })

      val localX = (4400000 / 10000) * 2 + xOffset
      g.setColor(Color.black)
//      g.fillRect(localX, 0, 2, 10)
      g.drawString("Variant", 2,18)
      g.drawString("LowCov", 2,28)
      ImageIO.write(img, "png", new File(workfolder + "/hawk.preprocess." + i + ".png"))

     
    }

  }

}