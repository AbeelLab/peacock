package peacock.figures

import java.io.File
import atk.compbio.tree.Tree
import peacock.core.LabelGenerator
import atk.util.Tool
import atk.util.ColorPalette
import atk.util.ColorTools
import peacock.vignets.PhenotypeGenotypeVignets
import peacock.vignets.CategoryVignets
import peacock.core.TreeViz
import peacock.core.VignetMaker
import peacock.core.FreeFormAddition
import peacock.vignets.TextVignets
import peacock.core.LabelGenerator
import peacock.vignets.BinaryVignets
import peacock.vignets.BarchartVignets
import peacock.vignets.LSPVignets
import peacock.vignets.ImageVignets
import peacock.vignets.HeatMapVignet
import java.awt.Color
import peacock.support.CountPerTypePrep

object EverythingFigure extends Tool {

  case class Config(
    val multiFile: Seq[(String, File)] = Seq(),
    val moreFields: Seq[String] = Seq(),
    val geno: File = null,
    val pheno: File = null,
    val binary: File = null,
    val binaryCoding: File = null,
    val outputPrefix: String = null,
    val pgg: File = null,
    val lineage: File = null,
    val disableLineageColoring: Boolean = false,
    val tree: String = null,
    val treeWidth: Int = 800,
    val order: File = null,
    val highlightFile: File = null,
    val labelFlag: Boolean = true,
    val categoryCoding: File = null,
    val debugLabels: Boolean = false,
    val bootstrapvalues: Boolean = false,
    val clusters: File = null,
    val clusterColoring: File = null)

  private val colorMap = List("LongDeletion" -> new Color(228, 26, 28), "LongInsertion" -> new Color(55, 126, 184),
    "LongSubstitution" -> new Color(77, 175, 74), "SingleDeletion" -> new Color(152, 78, 163), "SingleInsertion" -> new Color(255, 127, 0), "SingleSubstitution" -> new Color(255, 255, 51)).toMap

  override val version = """
      2014/11/28	Initial version with version information
                    Added option to display multiple category values
      2014/12/15	Added flag to disable label output
                    Reorganized internals so that extra and category displays appear in the order 
                    they are specified on the CLI
      2014/12/16	Moved CPT figures as option into this viz.
    				Included lineage coloring
      2015/03/05    Removed tree labels by default
    				Added option to display tree labels as debug information.
    				Added config option to disable lineage coloring
      2015/05/08	Support for LSP data from Emu
    				Removed Manhattan support
    				Removed Manhattan integrated tree labels
      2015/09/04    Added support for heat maps
      """

  def main(args: Array[String]): Unit = {

    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar magic") {
      /* Output prefix */
      opt[String]('o', "output") required () action { (x, c) => c.copy(outputPrefix = x) } text ("File prefix for the output files.")

      /* Phylogenetic tree */
      opt[String]('t', "tree") required () action { (x, c) => c.copy(tree = x) } text ("File containing the phylogenetic tree in NWK format.")
      opt[Unit]("no-labels") action { (x, c) => c.copy(labelFlag = false) } text ("Do not include sample identifier labels")
      opt[Int]("tree-width") action { (x, c) => c.copy(treeWidth = x) } text ("Display width of the tree. Default=800")

      /* Labeling information */
      opt[File]("pgg") action { (x, c) => c.copy(pgg = x) } text ("File containing principal genetic group information.")
      opt[File]("lineage") action { (x, c) => c.copy(lineage = x) } text ("File containing lineage information, by default this is generated from the spoligotypes.")
      opt[Unit]("disable-lineage-colors") action { (x, c) => c.copy(disableLineageColoring = true) } text ("Disable lineage specific coloring")

      /* Genotype-phenotype figure */
      opt[File]("phenotype") action { (x, c) => c.copy(pheno = x) } text ("File containing the phenotypes in matrix format")
      opt[File]("genotype") action { (x, c) => c.copy(geno = x) } text ("File containing the genotypes in matrix format (can be omitted, even when pheno is present).")
      opt[File]("order") action { (x, c) => c.copy(order = x) } text ("Display order and labels for columns in phenotype/genotype matrix")

      /* Matrices and column text */
      opt[File]("extra") unbounded () action { (x, c) => c.copy(multiFile = c.multiFile :+ ("label", x)) } text ("Additional labels. Each file should be key\tvalues")
      opt[String]("field") unbounded () action { (x, c) => c.copy(moreFields = c.moreFields :+ x) } text ("Additional field values to extract from Manhattan.")

      /*
       * AP 
       */
      opt[File]("binary") action { (x, c) => c.copy(binary = x) } text ("Binary AP matrix")
      opt[File]("binary-coding") action { (x, c) => c.copy(binaryCoding = x) } text ("Color coding for binary values, default black/white")
      /*
       * Category visual encoding 
       */
      opt[File]("category") unbounded () action { (x, c) => c.copy(multiFile = c.multiFile :+ ("category", x)) } text ("Categorical values")
      opt[File]("category-coding") action { (x, c) => c.copy(categoryCoding = x) } text ("Categorical color coding")

      /*
       * Heatmap
       */
      opt[File]("heatmap") unbounded () action { (x, c) => c.copy(multiFile = c.multiFile :+ ("heatmap", x)) } text ("Heatmap matrix file with values [0,1]")
      /*
       * CPT
       */
      opt[File]("cpt") unbounded () action { (x, c) => c.copy(multiFile = c.multiFile :+ ("cpt", x)) } text ("Count per type summary of a VCF file, created by the 'prep-cpt' command.")

      /*
       * LSPs
       */
      opt[File]("lsp") unbounded () action { (x, c) => c.copy(multiFile = c.multiFile :+ ("lsp", x)) } text ("A/P of LSPs. ")

      /*
         * Sample highlight
         */

      opt[File]("highlight-file") action { (x, c) => c.copy(highlightFile = x) } text ("Highlight the samples that occur in this file.")

      /*
       * Bootstrap values
       */
      opt[Unit]("bootstrap-values") action { (x, c) => c.copy(bootstrapvalues = true) } text ("File contains bootstrap values.")

      /*
       * Add cluster file
       */
      opt[File]("clusters") action { (x, c) => c.copy(clusters = x) } text ("File containing cluster information")
      opt[File]("cluster-colors") action { (x, c) => c.copy(clusterColoring = x) } text ("File containing cluster colors.")

      /**
       * Debug options
       *
       */
      opt[Unit]("debug-labels") action { (x, c) => c.copy(debugLabels = true) } text ("Include tree labels as additional labeling.")

    }
    parser.parse(args, Config()) map { config =>

      val tree = new Tree(config.tree)

      val highlights = if (config.highlightFile != null) tLines(config.highlightFile) else List.empty[String]

      val labels = new LabelGenerator

      var vignetList = Seq[VignetMaker]()
      var freeformList = Seq[FreeFormAddition]()
      /**
       * Geno-type phenotype matrix
       */
      if (config.geno != null && config.pheno != null) {
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
        val pgVignets = new PhenotypeGenotypeVignets(config.pheno, config.geno, sorted)
        vignetList = vignetList :+ pgVignets
        freeformList = freeformList :+ pgVignets.pgLegend
      }

      config.multiFile.toList.map {
        _ match {
          case ("category", x) =>
            val dataMap = tMap(tLines(x))

            val colorMap = if (config.categoryCoding != null) {
              assume(config.categoryCoding.exists())
              tMap(tLines(config.categoryCoding)).mapValues(f => {
                //      val arr = f.split("\t")
                ColorTools.decodeColor(f)
                //      assume(arr.size==3,"Incorrect number of values for RGB: "+arr.toList)
                //      new Color(arr(0).toInt, arr(1).toInt, arr(2).toInt)
              })
            } else {
              val list = dataMap.values.toSet.toList
              (for (key <- list.zipWithIndex) yield {
                key._1 -> ColorPalette(key._2)
              }).toMap

            }

            vignetList = vignetList :+ new CategoryVignets(dataMap, colorMap)

          case ("label", x) =>

            val mapping = tMap(tLines(x))
            // println(mapping.toList.take(5))
            vignetList = vignetList :+ new TextVignets(mapping.getOrElse("$$", "").split("\t").toList, mapping)

          case ("heatmap", x) =>
            val rawMap=tMap(tLines(x))
            val mapping = rawMap.filterNot(_._1.equals("$$")).mapValues(line=>{
            	line.split("\t").map(_.toDouble).toList
            })
            
          
            vignetList = vignetList :+ new HeatMapVignet(rawMap.getOrElse("$$", "No header information").split("\t").toList, mapping)

          case ("cpt", x) =>
            val inData = tMap(tLines(x)).mapValues { f =>
              val pairs = CountPerTypePrep.keys.zip(f.split("\t"))
              pairs.toMap.mapValues(_.toInt)

            }

            val rowSums = inData.map(p => p._1 -> (p._2).map(_._2).sum).toMap
            val max = rowSums.map(_._2).max

            println("Max sum=" + max)

            val barWidth = 650.0f
            val barRatio = barWidth / max

            println("Bar ratio=" + barRatio)

            println("Indata.size=" + inData.size)
            vignetList = vignetList :+ new BarchartVignets(inData, colorMap)
          case ("lsp", x) =>
            vignetList = vignetList :+ new LSPVignets(x)

          case ("image", x) =>
            val fileMapping = tMap(tLines(x)).mapValues(new File(_))
            vignetList = vignetList :+ new ImageVignets(fileMapping, 950, 30)
        }
      }

      if (config.binary != null) {
        vignetList = vignetList :+ new BinaryVignets(config.binary, colorCoding = config.binaryCoding)
      }

      val labelListX1 = if (config.labelFlag) List(labels) else List.empty[LabelGenerator]

      val labelList =
        if (config.debugLabels)
          List(new LabelGenerator) ++ labelListX1
        else labelListX1

      TreeViz.make(tree, treeWidth = config.treeWidth, freeForm = freeformList.toList, labels = labelList, vignets = vignetList.toList, exportPrefix = config.outputPrefix + "peacock.magic.", highlights = highlights, lineage = config.lineage, lineageColoring = (!config.disableLineageColoring), bootstrap = config.bootstrapvalues, clusters = config.clusters, clusterColoring = config.clusterColoring)

      //      TreeViz.make(tree, treeWidth = config.treeWidth, freeForm = freeformList.toList, labels = labelList, vignets = vignetList.toList, exportPrefix = config.outputPrefix + "peacock.magic.", highlights = highlights, lineage = config.lineage, lineageColoring = (!config.disableLineageColoring))

    }

  }

}
