package peacock.core

import atk.util.Tool
import atk.compbio.tree.Tree
import processing.core.PApplet
import processing.core.PGraphics
import processing.core.PConstants
import scala.collection.JavaConversions._
import peacock.vignets.DualHeatMap
import java.io.File

object DualTreeMatrixViz extends Tool {
  case class Config(val gradient:Int= -1,val heatmap: File = null, val outputPrefix: String = null, val pgg: File = null, val lineage: File = null, val tree: File = null)

  def main(args: Array[String]) {
   
    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar dual [options]") {
      opt[String]('o', "output") required () action { (x, c) => c.copy(outputPrefix = x) } text ("File prefix for the output files.")
      opt[File]("heatmap") required () action { (x, c) => c.copy(heatmap = x) } text ("Matrix to visualize as heatmap") validate { x => if (x.exists) success else failure("Matrix file not found: " + x) }
opt[Int]("gradient") action { (x, c) => c.copy(gradient = x) } text ("Set gradient length")
      
      
      opt[File]('t', "tree") required () action { (x, c) => c.copy(tree = x) } text ("File containing the phylogenetic tree in NWK format.") validate { x => if (x.exists) success else failure("NWK tree file not found: " + x) }

      opt[File]("pgg") action { (x, c) => c.copy(pgg = x) } text ("File containing principal genetic group information.") validate { x => if (x.exists) success else failure("PGG file not found: " + x) }

      opt[File]("lineage") action { (x, c) => c.copy(lineage = x) } text ("File containing lineage information.") validate { x => if (x.exists) success else failure("Spoligotype file not found: " + x) }
     
    }
    parser.parse(args, Config()) map { config =>

      val tree = new Tree(config.tree.toString())

      val labels =new LabelGenerator

      make(tree, 500, labels = labels, vignets = new DualHeatMap(config.heatmap,config.gradient), exportPrefix = config.outputPrefix + ".peacock^2.heatmap.")

    }

  }

  implicit def toLeftList[List[T], T](left: List[T]): Either[List[T], T] = Left(left)

  implicit def toRightList[List[T], T](right: T): Either[List[T], T] = Right(right)

  def make(tree: Tree,
    treeWidth: Int = 400,
    labels: Either[List[LabelGenerator], LabelGenerator] = List(new LabelGenerator),
    vignets: DualVignetMaker = new DualVignetMaker,
    exportPrefix: String = "peacock^2.",
    freeForm: Either[List[FreeFormAddition], FreeFormAddition] = List(new FreeFormAddition)) {

    val fLabels = labels.fold(identity, List(_))

    val fFreeform = freeForm.fold(identity, List(_))

    val embed = new DualTreeMatrixViz(tree, treeWidth, fLabels, vignets, exportPrefix, fFreeform);
    //
    //    frame.add(new JScrollPane(embed), BorderLayout.CENTER);

    // important to call this whenever embedding a PApplet.
    // It ensures that the animation thread is started and
    // that other internal variables are properly set.
    embed.init();
    embed.setup
    //    embed.draw
    //    embed.save(exportPrefix + timestamp + ".png")
    embed.dispose()

  }

}

class DualTreeMatrixViz(val tree: Tree, val treeWidth: Int, labels: List[LabelGenerator], vignets: DualVignetMaker, val exportPrefix: String, val freeForm: List[FreeFormAddition]) extends PApplet with Tool {

  override def setup() {
    
    val treep = new TreePImage(tree, treeWidth, labels, List.empty[VignetMaker]);
    val width = tree.getLeafCount() * vignets.x + treep.totalWidth
    val height = tree.getLeafCount() * vignets.y + treep.totalWidth

    println("W = " + width)
    println("H = " + height)

    var pdf: PGraphics = this.createGraphics(width, height, PConstants.PDF, exportPrefix + timestamp + ".pdf");

    pdf.beginDraw()
    pdf.resetMatrix()
    pdf.background(color(255))

    pdf.fill(255)
    pdf.rect(0, 0, width, height)
    pdf.fill(0)
    
    vignets.legend(pdf)
    
    pdf.translate(0, treep.totalWidth)
    pdf.pushMatrix()
    treep.render(pdf);
    pdf.popMatrix()

    pdf.pushMatrix()
    pdf.translate(treep.totalWidth, -treep.totalWidth)
    pdf.rotate(PConstants.PI / 2)
    pdf.scale(1, -1)
    treep.render(pdf, true);

    pdf.popMatrix()

    pdf.resetMatrix()
    pdf.translate(treep.totalWidth, treep.totalWidth)

    /* Ready to go */
    //    pdf.fill(pdf.color(255, 0, 0))
    //    pdf.rect(0, 0, 100, 100)

    for (a <- 0 until tree.getLeafCount()) {
      val x = tree.getLeaves(tree.root)(a)
      for (b <- 0 until tree.getLeafCount()) {
        val y = tree.getLeaves(tree.root)(b) //tree.getLeaf(b)
        println(x.getName() + "\t" + y.getName() + "\t" + (a * vignets.x) + "\t" + b * vignets.y)
        pdf.pushMatrix()
        pdf.translate(a * vignets.x, b * vignets.y)
        vignets.image(pdf, x.getName(), y.getName())

        pdf.popMatrix()
      }

    }

    freeForm.map(f => {
      pdf.pushMatrix()
      f.drawFreeForm(pdf)
      pdf.popMatrix()
    })
    pdf.endDraw();
    pdf.dispose();

  }
}