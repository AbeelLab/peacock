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

object TableViz extends Tool {

  implicit def toLeftList[List[T], T](left: List[T]): Either[List[T], T] = Left(left)

  implicit def toRightList[List[T], T](right: T): Either[List[T], T] = Right(right)

  def make(sorting:List[String],
    labels: Either[List[LabelGenerator], LabelGenerator] = List(new LabelGenerator),
    vignets: Either[List[VignetMaker], VignetMaker] = List(new VignetMaker),
    output: String = "peacock.",
    freeForm: Either[List[FreeFormAddition], FreeFormAddition] = List(new FreeFormAddition),
    highlights:List[String]=List.empty[String]) {

    val fLabels = labels.fold(identity, List(_))
    val fVignets = vignets.fold(identity, List(_))
    val fFreeform = freeForm.fold(identity, List(_))

    val embed = new TableViz(sorting, fLabels, fVignets, output, fFreeform,highlights);
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

class TableViz(sorting:List[String], labels: List[LabelGenerator], vignets: List[VignetMaker], val exportPrefix: String, val freeForm: List[FreeFormAddition],val highlights:List[String]) extends PApplet with Tool {

  override def setup() {
    val treep = new TablePImage(sorting, labels, vignets,highlights,this);
    val factor=PGraphicsPDF.RESCALE_FACTOR
    var pdf: PGraphics = this.createGraphics((treep.totalWidth/factor).toInt, (treep.totalHeight/factor).toInt, PConstants.PDF, exportPrefix + timestamp + ".pdf");

    pdf.beginDraw()
   
    pdf.background(color(255))

    pdf.fill(255)
    pdf.rect(0, 0, treep.totalWidth, treep.totalHeight)
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