package peacock.core

import processing.core.PImage
import processing.core.PApplet
import javax.swing.JFrame
import java.awt.BorderLayout
import javax.swing.JScrollPane
import javax.swing.WindowConstants
import java.awt.Dimension
import atk.util.Tool
import scala.util.Either
import atk.compbio.tree.Tree
import processing.pdf._
import processing.core.PConstants
import processing.core.PGraphics
import peacock.support.PTools
import java.io.File

object TreeViz extends Tool {

  implicit def toLeftList[List[T], T](left: List[T]): Either[List[T], T] = Left(left)

  implicit def toRightList[List[T], T](right: T): Either[List[T], T] = Right(right)

  def make(tree: Tree,
    treeWidth: Int = 400,
    labels: Either[List[LabelGenerator], LabelGenerator] = List(new LabelGenerator),
    vignets: Either[List[VignetMaker], VignetMaker] = List(new VignetMaker),
    exportPrefix: String = "peacock.",
    freeForm: Either[List[FreeFormAddition], FreeFormAddition] = List(new FreeFormAddition),
    highlights:List[String]=List.empty[String],
    lineage:File=null,
    lineageColoring:Boolean=true,
    bootstrap: Boolean = false,
    clusters: File = null,
    clusterColoring: File = null) {

    val fLabels = labels.fold(identity, List(_))
    val fVignets = vignets.fold(identity, List(_))
    val fFreeform = freeForm.fold(identity, List(_))

    val embed = new TreeViz(tree, treeWidth, fLabels, fVignets, exportPrefix, fFreeform,highlights,lineage,lineageColoring, bootstrap, clusters, clusterColoring);
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
    PTools.dispose();
  }

}

class TreeViz(val tree: Tree, val treeWidth: Int, labels: List[LabelGenerator], vignets: List[VignetMaker], val exportPrefix: String, val freeForm: List[FreeFormAddition],val highlights:List[String],val lineage:File,val lineageColoring:Boolean, val bootstrap: Boolean, val clusters: File, val clusterColoring: File) extends PApplet with Tool {

  override def setup() {
    
    val lineageX=SharedViz.readLineage(lineage)
    
    val clustersX = if (clusters != null) tMap(tLines(clusters), keyColumn=0, valueColumn=1).mapValues(str => str.split("\t").toList) else Map.empty[String, List[String]]    
    val clusterColoringX = if (clusterColoring != null) tMap(tLines(clusterColoring), keyColumn=0, valueColumn=1) else Map.empty[String, String]
    
    val treep = new TreePImage(tree, treeWidth, labels, vignets,highlights,lineageX, clustersX, clusterColoringX);
    val factor=PGraphicsPDF.RESCALE_FACTOR
    var pdf: PGraphics = this.createGraphics((treep.totalWidth/factor).toInt, (treep.totalHeight/factor).toInt, PConstants.PDF, exportPrefix + timestamp + ".pdf");
   
    pdf.beginDraw()
   
    pdf.background(color(255))

    pdf.fill(255)
    pdf.rect(-1, -1, treep.totalWidth+2, treep.totalHeight+2)
    pdf.fill(0)
    treep.render(pdf);
    freeForm.map(f => {
      pdf.pushMatrix()
      f.drawFreeForm(pdf)
      pdf.popMatrix()
    })
    pdf.endDraw();
    pdf.dispose();

  }
}