package peacock.figures

import atk.util.Tool
import peacock.core.FreeFormAddition
import java.io.File
import processing.core.PGraphics
import java.io.PrintWriter
import atk.compbio.tree.Tree
import peacock.core.TreeViz
import peacock.vignets.BinaryVignets
import processing.core.PImage
import tbarc.drugs.Drug
import scala.io.Source
import peacock.core.LabelGenerator
import java.text.NumberFormat
import java.util.Locale
import peacock.vignets.MetaBinaryVignets
import tbarc.drugs.DrugMapAPI

object MutsAP extends Tool {


  class Label(str: String) extends FreeFormAddition {
    override def drawFreeForm(g: PGraphics) {
      val textSize = 75
      var offset = textSize
      g.textSize(textSize)
      g.fill(0)
      g.stroke(0)
      println("plotting: " + str)
      str.split("\\$").map { f =>
        g.text(f.replaceAll("[_]+", " "), 50, offset)
        offset += textSize
      }
    }

  }

  case class Config(val minimum: Int = 0, val low: Boolean = false,
    val syn: Boolean = false, val drug: File = null,
    val amb: Boolean = false, val inputCCLM: File = null, val inputMatrix: File = null,
    val outputPrefix: String = null, val pgg: File = null,
    val lineage: File = null, val tree: File = null
   )

  def main(args: Array[String]): Unit = {

  
    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar phenotype-mutation-overlay") {
      opt[File]("input-cclm") action { (x, c) => c.copy(inputCCLM = x) } text ("Input file [output_prefix].snps_to_known_genes.txt") //, { v: String => config.spacerFile = v })
      opt[File]("input-matrix") action { (x, c) => c.copy(inputMatrix = x) } text ("Input file in matrix format that has AP per mutations or per item of interest") //, { v: String => config.spacerFile = v })
      opt[String]('o', "output") required () action { (x, c) => c.copy(outputPrefix = x) } text ("File prefix for the output files.")

      opt[File]('t', "tree") required () action { (x, c) => c.copy(tree = x) } text ("File containing the phylogenetic tree in NWK format.")
      opt[Int]("minimum") action { (x, c) => c.copy(minimum = x) } text ("Filter to exclude variants that occur less than N times. Default N = 0 (currently ignored)")

      opt[File]('d', "drug") required () action { (x, c) => c.copy(drug = x) } text ("File containing the drug information, [drugs.subset]")

      opt[File]("pgg") action { (x, c) => c.copy(pgg = x) } text ("File containing principal genetic group information.")
      opt[File]("lineage") action { (x, c) => c.copy(lineage = x) } text ("File containing lineage information, by default this is generated from the spoligotype.")
    
      opt[Unit]("syn") action { (x, c) => c.copy(syn = true) } text ("Include synonymous variants")
      opt[Unit]("amb") action { (x, c) => c.copy(amb = true) } text ("Include ambiguous variants")
      opt[Unit]("low") action { (x, c) => c.copy(low = true) } text ("Include low coverage variants")

    }
    parser.parse(args, Config()) map { config =>

      val matrix = getMatrix(config)

      paint(config, matrix)

    }

  }

  private def getMatrix(config: Config): File = {
    assume(config.inputCCLM != null || config.inputMatrix != null, "You need to provide either CCLM or matrix input")
    if (config.inputMatrix != null) {
      config.inputMatrix
    } else {
      val a1 = tMap(tLines(config.inputCCLM))

      val map = a1.mapValues(f => {
        f.split(";").filterNot(g =>
          !config.syn && g.contains(":syn")).filterNot(h =>
          !config.amb && h.contains(",amb")).filterNot(i =>
          !config.low && i.contains(",low")).mkString(";")
      })
      println("mapping: " + map)

      /**
       * Build AP matrix for mutations
       */
      val set1 = map.map(f => f._2.split(";").toList).toList.flatten.toSet.toList //.filter(f => genes.contains(f.split(":")(0)))
      val set = if (set1(0).split(",").size > 3) {
        set1.sortBy(f => {
          val split = f.split(",")
          //    	   assume(split.size>3,"Not enough field: "+split.toList)
          if (split.size > 3)
            split(3).toInt
          else -1
        })
      } else set1.sortBy(identity)
      val outputFile = new File(config.outputPrefix + ".apknownmatrix.matrix")
      val pw = new PrintWriter(outputFile)
      pw.println(generatorInfo)
      pw.println("$$\t" + set.mkString("\t"))
      for (e <- map) {
        val p = e._2.split(";").toSet
        pw.println(e._1 + "\t" + set.map(f => if (p.contains(f)) "1" else "0").mkString("\t"))
      }
      pw.close
      
      /**
       * Filter based on minimum occurences
       *
       *
       */
      val mat = tLines(config.outputPrefix + ".apknownmatrix.matrix").map(l => l.split("\t").toList).transpose
      val filtered = mat.filter(row => {
        println("R: " + row.take(10))
        row(0).equals("$$") || row.drop(1).map(_.toInt).fold(0)((a, b) => a + b) >= config.minimum
      }).transpose
      val pwx = new PrintWriter(config.outputPrefix + ".apknownmatrix.filtered.matrix")
      pwx.println(generatorInfo)
      pwx.println(filtered.map(line => line.mkString("\t")).mkString("\n"))
      pwx.close
      
      outputFile
    }
    
      
    

  }

  private def paint(config: Config, inputMatrix: File) {
    val drugs = new DrugMapAPI(config.drug)
    /**
     * Generate tables and figures per drug
     */

    for (drug <- drugs.drugIdentifiers) yield {
      /* */
      val unifiedDrug = drug.split("\\$")(0)

      /**
       * Output detailed campbell like table
       */

      val pwX = new PrintWriter(config.outputPrefix + ".genopheno." + drug + ".summary.txt")
      pwX.println(generatorInfo)
      pwX.println("# Genotype/phenotype")
      pwX.println("# drug \tRR\tSR\tRS\tSS\tsensitivity\tspecificity\tF")
      val list = List("11", "01", "10", "00")
      val lines = tLines(inputMatrix).map(l => l.split("\t").toList)
      val extendedLines = List(lines(0)) ++ (for {
        line <- lines.drop(1)
        val profile = drugs.drugMap.getOrElse(line(0), Map.empty[String, Char])
        val strainResistant = profile.getOrElse(drug, '-') match {
          case 'R' => '1'
          case 'S' => '0'
          case _ => '-'
        }
        val mm = line.drop(1).map(f => f + strainResistant)

      } yield List(line(0)) ++ mm)

      println(extendedLines.take(4).mkString("\n"))

      val matrix = extendedLines.transpose

      for (mutationLine <- matrix) {
        //        	println("ML = "+mutationLine)

        val sum = mutationLine.drop(1).groupBy(identity).mapValues(_.size)
        val values = list.map(sum.getOrElse(_, 0))
        //      val sen = values(0) / (values(0).toDouble + values(1))
        //      val spec = values(3) / (values(2).toDouble + values(3))
        val tp = values(0).toDouble
        val fn = values(1).toDouble
        val fp = values(2).toDouble
        val tn = values(3).toDouble

        val sen = tp / (tp + fn)
        val spec = tn / (fp + tn)
        //          val acc = (tp + tn) / (tp + tn + fn + fp)
        val F = (2 * tp) / (2 * tp + fp + fn)
        pwX.println(mutationLine(0) + "\t" + values.mkString("\t") + "\t" + nfP.format(sen) + "\t" + nfP.format(spec) + "\t" + nfP.format(F))
      }
      pwX.close
      //        System.exit(0)

    

      /**
       * Output tree figure
       */

      val tree = new Tree(config.tree.toString())
     
      val dr = new MetaBinaryVignets(inputMatrix, drug, drugs)

      val labels = new LabelGenerator

      TreeViz.make(tree, treeWidth = 800, labels = labels, vignets = List(dr), exportPrefix = config.outputPrefix + ".peacock.mutationAP." + drug + ".", freeForm = new Label(drug))
     
    }
  }

  

}

