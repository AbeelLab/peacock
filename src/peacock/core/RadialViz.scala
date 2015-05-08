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

object RadialViz extends Tool {

  implicit def toLeftList[List[T], T](left: List[T]): Either[List[T], T] = Left(left)

  implicit def toRightList[List[T], T](right: T): Either[List[T], T] = Right(right)

  def make(tree: Tree,
    treeWidth: Int = 2048,
    labels: Either[List[LabelGenerator], LabelGenerator] = List(new LabelGenerator),
    vignets: Either[List[VignetMaker], VignetMaker] = List(new VignetMaker),
    exportPrefix: String = "peacockR.",
    freeForm: Either[List[FreeFormAddition], FreeFormAddition] = List(new FreeFormAddition),
    highlights: List[String] = List.empty[String],
    lineage:File=null  ) {

    val fLabels = labels.fold(identity, List(_))
    val fVignets = vignets.fold(identity, List(_))
    val fFreeform = freeForm.fold(identity, List(_))

    val embed = new RadialViz(tree, treeWidth, fLabels, fVignets, exportPrefix, fFreeform, highlights,lineage);
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

class RadialViz(val tree: Tree, val treeWidth: Int, labels: List[LabelGenerator], vignets: List[VignetMaker], val exportPrefix: String, val freeForm: List[FreeFormAddition], val highlights: List[String],val lineage:File) extends PApplet with Tool {

  override def setup() {
     val lineageX=if(lineage!=null)tMap(tLines(lineage),keyColumn=0,valueColumn=3,limitSplit=false)else Map.empty[String,String]
    val treep = new RadialPImage(tree, treeWidth, labels, vignets, highlights,lineageX);
    val factor = PGraphicsPDF.RESCALE_FACTOR
    val g = this.createGraphics((treep.totalWidth / factor).toInt, (treep.totalHeight / factor).toInt, PConstants.PDF, exportPrefix + timestamp + ".pdf")
    val pdf: PGraphicsPDF = g.asInstanceOf[PGraphicsPDF]

    pdf.beginDraw()
   
    pdf.background(color(255))

    pdf.fill(255)
    pdf.rect(-1, -1, treep.totalWidth + 2, treep.totalHeight + 2)
    pdf.fill(0)
    treep.render(pdf);
    freeForm.map(f => {
      pdf.pushMatrix()
      f.drawFreeForm(pdf)
      pdf.popMatrix()
    })

    pdf.nextPage()
    pdf.scale(1 / factor)
    val text = List("RadialViz, a Peacock visualization (C) Thomas Abeel") ++ generatorInfo.split("\n") ++ labels.map(_.toString) ++ vignets.map(_.toString) ++ freeForm.map(_.toString()) ++ List("highlights: " + highlights.mkString(","), "export: " + exportPrefix)

    text.map(_.split("\n")).flatten.map { f =>
      pdf.text(f.replaceAll("\t", "     "), 100, 100)
      pdf.translate(0, 15)

    }

    pdf.endDraw();
    pdf.dispose();

  }
}