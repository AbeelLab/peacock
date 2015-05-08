package peacock.figures

import java.io.File

import atk.compbio.tree.Tree
import peacock.core.LabelGenerator
import atk.util.Tool
import peacock.vignets.PhenotypeGenotypeVignets
import peacock.vignets.CategoryVignets
import peacock.core.RadialViz
import peacock.core.VignetMaker
import peacock.core.FreeFormAddition
import peacock.vignets.TextVignets
import peacock.core.LabelGenerator
import peacock.vignets.BinaryVignets

object RadialEverythingFigure extends Tool {

  override val version="""
    pre 2015:        Initial version used for TB-ARC work, including later improvements such as lineage coloring
    2015/01/09       Fix NPE when not providing lineage information
    """
  
  case class Config(
    val moreLabels: Seq[File] = Seq(),
    val moreFields: Seq[String] = Seq(),
    val geno: File = null,
    val pheno: File = null,
    val binary: File = null,
    val outputPrefix: String = null,
    val pgg: File = null,
    val lineage: File = null,
    val tree: String = null,
    val order: File = null,
    val highlightFile: File = null,
    val category: Seq[File] = Seq(),
    val categoryCoding: File = null,
    val figureSize:Int=1600
  )

  def main(args: Array[String]): Unit = {

    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar radial") {
      /* Output prefix */
      opt[String]('o', "output") required () action { (x, c) => c.copy(outputPrefix = x) } text ("File prefix for the output files.")
      
      opt[Int]("size") action { (x, c) => c.copy(figureSize = x) } text ("Canvas size for figure, default 1600 units")

      /* Phylogenetic tree */
      opt[String]('t', "tree") required () action { (x, c) => c.copy(tree = x) } text ("File containing the phylogenetic tree in NWK format.")

      /* Labeling information */
      opt[File]("pgg") action { (x, c) => c.copy(pgg = x) } text ("File containing principal genetic group information.")
      opt[File]("lineage") action { (x, c) => c.copy(lineage = x) } text ("File containing lineage information, by default this is generated from the spoligotype.")
      
      /* Genotype-phenotype figure */
      opt[File]("phenotype") action { (x, c) => c.copy(pheno = x) } text ("File containing the phenotypes in matrix format")
      opt[File]("genotype") action { (x, c) => c.copy(geno = x) } text ("File containing the genotypes in matrix format (can be omitted, even when pheno is present).")
      opt[File]("order") action { (x, c) => c.copy(order = x) } text ("Display order and labels for columns in phenotype/genotype matrix")

      /* Matrices and column text */
      opt[File]("extra") unbounded () action { (x, c) => c.copy(moreLabels = c.moreLabels :+ x) } text ("Additional labels. Each file should be key\tvalues")
//      opt[String]("field") unbounded () action { (x, c) => c.copy(moreFields = c.moreFields :+ x) } text ("Additional field values to extract from Manhattan.")

      /*
       * AP 
       */
      opt[File]("binary") action { (x, c) => c.copy(binary = x) } text ("Binary AP matrix")

      /*
       * Category visual encoding 
       */
      opt[File]("category") unbounded () action { (x, c) => c.copy(category = c.category :+ x) } text ("Categorical values")
      opt[File]("category-coding") action { (x, c) => c.copy(categoryCoding = x) } text ("Categorical color coding")

      /*
         * Sample highlight
         */
      opt[File]("highlight-file") action { (x, c) => c.copy(highlightFile = x) } text ("Highlight the samples that occur in this file")
    }
    parser.parse(args, Config()) map { config =>

      val tree = new Tree(config.tree)

      val highlights = if (config.highlightFile != null) tLines(config.highlightFile)/*.map(GNumbers.singleG(_))*/ else List.empty[String]

      val labels = new LabelGenerator

      var vignetList = Seq[VignetMaker]()
      var freeformList = Seq[FreeFormAddition]()
      /**
       * Geno-type phenotype matrix
       */
      if (config.geno != null || config.pheno != null) {
        /*
       * Default column sorting
       * Sorted is a list of pairs where the first item is the key, the second the display name. The order determines the display order of the columns
       */
        val sorted = if (config.order == null) {
          null
        } else {
          tLines(config.order).map { line =>
            val arr = line.split("\t", 2)
            arr(0) -> arr(1)

          }
        }

        println("Sorted: " + sorted)

        //      System.exit(0)
        val pgVignets = new PhenotypeGenotypeVignets(config.pheno, config.geno, sorted,true)
        vignetList = vignetList :+ pgVignets
        freeformList = freeformList :+ pgVignets.pgLegend
      }
      /**
       * Text matrices and extra text fields
       */


      config.moreLabels.toList.map { file =>
        val mapping = tMap(tLines(file))
        // println(mapping.toList.take(5))
        vignetList = vignetList :+ new TextVignets(mapping.getOrElse("$$", "").split("\t").toList, mapping)
      }

      if (config.binary != null) {
        vignetList = vignetList :+ new BinaryVignets(config.binary)
      }

      config.category.toList.map { file =>
        vignetList = vignetList :+ new CategoryVignets(file, config.categoryCoding,45,15)
      }
     
      RadialViz.make(tree, treeWidth = config.figureSize, freeForm = freeformList.toList, labels = List(new LabelGenerator, labels), vignets = vignetList.toList, exportPrefix = config.outputPrefix + "peacockR.magic.", highlights = highlights,lineage=config.lineage)

    }

  }

}