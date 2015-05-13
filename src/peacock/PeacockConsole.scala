package peacock


import peacock.support.SplitGenoPheno
import peacock.figures.LorikeetFigure
import peacock.support.HawkPrepare
import peacock.figures.MutsAP
import peacock.figures.MutsAPKnownDRgenes
import peacock.support.CCLM2matrix
import peacock.core.DualTreeMatrixViz
import peacock.figures.EverythingFigure
import peacock.figures.RadialEverythingFigure
import peacock.figures.MICfigure
import processing.core.PGraphics
import processing.pdf.PGraphicsPDF

object PeacockConsole {

  def main(args: Array[String]): Unit = {

    PGraphicsPDF.RESCALE_FACTOR=16f
    if (args.length == 0) {

      listInstructions
    } else {
      args(0) match {
        case "list" => listInstructions
        case "help" => listInstructions
        
        case "spoligotype" => LorikeetFigure.main(args.drop(1))
        case "prep-genotype-phenotype" => SplitGenoPheno.main(args.drop(1))
        case "genotype-phenotype-details" => MutsAPKnownDRgenes.main(args.drop(1))
        case "phenotype-mutation-overlay" => MutsAP.main(args.drop(1))
        case "cclm-matrix" => CCLM2matrix.main(args.drop(1))
        case "cclm2matrix" => CCLM2matrix.main(args.drop(1))
        case "prep-hawk" => HawkPrepare.main(args.drop(1))
        case "dual" => DualTreeMatrixViz.main(args.drop(1))
        case "magic" => EverythingFigure.main(args.drop(1))
        case "radial" => RadialEverythingFigure.main(args.drop(1))
        case "mic-table" => MICfigure.main(args.drop(1))
        case "_" => listInstructions
      }
    }

  }

  def listInstructions() {
    println("Usage:java -jar peacock.jar [instruction] [instruction options...]")
    println("Instructions:")
    
    println("\tspoligotype                  Spoligotype visualizations")
    println("")
    println("")
    
    println("\tprep-cpt                     Counts per type visualization pre-processing")
    println("\tcpt                          Counts per type visualizations")
    
    println("")
    println("\tprep-genotype-phenotype      Split genotype-phenotype matrix in preparation of concordance figure")
    println("\tgenotype-phenotype-details   Genotype phenotype figure with details information of mutations in known associated genes.")
    println("\tphenotype-mutation-overlay   Genotype phenotype figure with details information of mutations a provided set of genes.")
    println("\tcclm-matrix                  Convert a cclm formatted file to a AP matrix.")
    println("\tlsp-overview                 Overview figure of LSPs")
    println("")
    println("\tprep-hawk                    Prepare images for Hawk visualization")
    println("")
    println("\tdual                         Dual tree matrix visualization")
    println("")
    println("\tmagic                        Undocumented program that does stuff.")
    println("\tradial                       Another undocumented program that does radial stuff.")
    println("\t")
    println("\tmic-table                    Generate a table view with MIC values, detailed mutations and DST information.")

  }

}
