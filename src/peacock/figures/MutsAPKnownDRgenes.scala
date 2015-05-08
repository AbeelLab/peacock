package peacock.figures


import atk.util.Tool
import tbarc.drugs.DrugMapAPI
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
import scala.collection.JavaConversions._

object MutsAPKnownDRgenes extends Tool{


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

  case class Config(val minimum: Int = 0, val low: Boolean = false, val syn: Boolean = false, val drug: File = null, val amb: Boolean = false, val input: File = null, val outputPrefix: String = null, val pgg: File = null, val lineage: File = null, val tree: File = null)

  def main(args: Array[String]): Unit = {

     val parser = new scopt.OptionParser[Config]("java -jar peacock.jar genotype-phenotype-detail") {
      opt[File]('i', "input") required () action { (x, c) => c.copy(input = x) } text ("Input file [output_prefix].snps_to_known_genes.txt") //, { v: String => config.spacerFile = v })

      opt[String]('o', "output") required () action { (x, c) => c.copy(outputPrefix = x) } text ("File prefix for the output files.")

      opt[File]('t', "tree") required () action { (x, c) => c.copy(tree = x) } text ("File containing the phylogenetic tree in NWK format.")
      opt[File]('d', "drug") required () action { (x, c) => c.copy(drug = x) } text ("File containing the drug information, [drugs.subset]")
      opt[Int]("minimum") action { (x, c) => c.copy(minimum = x) } text ("Filter to exclude variants that occur less than N times. Default N = 0 (currently ignored)")
      opt[File]("pgg") action { (x, c) => c.copy(pgg = x) } text ("File containing principal genetic group information.")

      opt[File]("lineage") action { (x, c) => c.copy(lineage = x) } text ("File containing lineage information, by default this is generated from the spoligotype.")
       opt[Unit]("syn") action { (x, c) => c.copy(syn = true) } text ("Include synonymous variants")
      opt[Unit]("amb") action { (x, c) => c.copy(amb = true) } text ("Include ambiguous variants")
      opt[Unit]("low") action { (x, c) => c.copy(low = true) } text ("Include low coverage variants")

    }
    parser.parse(args, Config()) map { config =>
      paint(config)

    }

  }

  private def paint(config: Config) {
    val drugs = new DrugMapAPI(config.drug)
    val a1 = tMap(tLines(config.input))

    val map = a1.mapValues(f => {
      f.split(";").filterNot(g =>
        !config.syn && g.contains(":syn")).filterNot(h =>
        !config.amb && h.contains(",amb")).filterNot(i =>
        !config.low && i.contains(",low")).mkString(";")
    })
    println("mapping: " + map.take(10).mkString("\n"))
    //System.exit(0)
    val gene2drugs = Source.fromInputStream(MutsAPKnownDRgenes.getClass().getResourceAsStream("/gene2drugs.txt")).getLines.toList.filterNot(f => f.startsWith("#")).filterNot(f => f.trim.size == 0)

    val drug2geneMap = gene2drugs.groupBy(f => { println(f); f.split("\\s+")(3) })

    val tree = new Tree(config.tree.toString())
    println("leaves: " + tree.getLeaves(tree.root).take(5))
    val leafList = tree.getLeaves(tree.root).map(l => l.getName()).toList

    println("d2g :" + drug2geneMap)

    println("D-keys: " + drug2geneMap.keys)
    println("I-keys: " + drugs.drugIdentifiers)
    //    System.exit(0)
    for (drug <- drugs.drugIdentifiers) yield {
      /* */
      val unifiedDrug = drug.split("\\$")(0)

      val m1 = drug2geneMap.getOrElse(unifiedDrug, List.empty[String])
      println("looking for: " + unifiedDrug + ",got " + m1)

      if (m1.size > 0) {

        val genes = m1.map(f => f.split("\\s+")(2))
        println("genes for " + drug + ": " + genes)
        val set1 = map.map(f => f._2.split(";").toList).toList.flatten.toSet.toList.filter(f => genes.contains(f.split(":")(0)))
        //val set = set1.sortBy(identity)
        println("Set1: " + set1)

        if (set1.size > 0) {
          val set = if (set1(0).split(",").size > 3) {
            set1.sortBy(f => {
              val split = f.split(",")
              //    	   assume(split.size>3,"Not enough field: "+split.toList)
              if (split.size > 3)
                split(3).toInt
              else -1
            })
          } else set1.sortBy(identity)
          val off=new File(config.outputPrefix + "apknownmatrix." + drug + ".matrix").getParentFile().mkdirs()
          val pw = new PrintWriter(config.outputPrefix + "apknownmatrix." + drug + ".matrix")
          pw.println(generatorInfo)
          pw.println("$$\t" + set.mkString("\t"))
          for (e <- map) {
            val p = e._2.split(";").toSet
            pw.println(e._1 + "\t" + set.map(f => if (p.contains(f)) "1" else "0").mkString("\t"))
          }
          pw.close

          /**
           * Output detailed campbell like table
           */

          val pwX = new PrintWriter(config.outputPrefix + "genopheno." + drug + ".summary.txt")
          pwX.println(generatorInfo)
          val list = List("11", "01", "10", "00")
          val linesIn = tLines(config.outputPrefix + "apknownmatrix." + drug + ".matrix").map(l => l.split("\t").toList)
          println(linesIn.take(4))
          //        System.exit(0)
          val lines = linesIn.filter(line => line(0).equals("$$") || leafList.contains(line(0)))
          val strainCount = (lines.size - 1)
          pwX.println("# " + strainCount + " strains included")

          pwX.println("# Genotype/phenotype")
          pwX.println("# drug \tRR\tSR\tRS\tSS\tsensitivity\tspecificity\tF\tsanity")

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

          for (mutationLine <- matrix.filterNot(_(0).equals("$$"))) {
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
            val sanity = (tp + fp + fn + tn).toInt
            assume(strainCount >= sanity, "Number of strains and sum of TP, FP, FN, TN does not match: " + strainCount + " !>= " + sanity)
            pwX.println(mutationLine(0) + "\t" + values.mkString("\t") + "\t" + nfP.format(sen) + "\t" + nfP.format(spec) + "\t" + nfP.format(F) + "\t" + sanity)
          }
          pwX.close
          //        System.exit(0)

          /**
           * Output tree figure
           */

          val dr = new DRVignets(new File(config.outputPrefix + "apknownmatrix." + drug + ".matrix"), drug, drugs)

          val labels = new LabelGenerator

          TreeViz.make(tree, treeWidth = 800, labels = labels, vignets = List(dr), exportPrefix = config.outputPrefix + "peacock.mutationAPKnownDRgenes." + drug + ".", freeForm = new Label(drug))
        }else{
          println("No mutations for: "+unifiedDrug)
        }
      } else {
        println("No data for: " + unifiedDrug)
      }
    }
  }

  class DRVignets(matrixFile: File, drug: String, drugs: DrugMapAPI) extends BinaryVignets(matrixFile) {

    override def image(buf: PGraphics, key: String) {
      buf.pushMatrix()
      buf.translate(4, 0)
      val l = matrix.getOrElse(key, List("Missing value: " + key))

      println("G for vignet: " + key)
      println("working with :" + drug)
      val profile = drugs.drugMap.getOrElse(key, Map.empty[String, Char])
      println("Profile: " + profile)

      val useL = l.zip(indices).sortBy(_._2)

      assume(profile != null)

      val strainResistant = profile.getOrElse(drug, 'U')

      var idx = 1
      for (s <- useL) {

        strainResistant match {
          case 'R' =>
            buf.stroke(buf.color(255, 202, 179))
            buf.fill(buf.color(255, 202, 179))

          case 'S' =>
            buf.stroke(buf.color(179, 179, 255))
            buf.fill(buf.color(179, 179, 255))
          case _ =>
            buf.stroke(buf.color(209, 209, 209))
            buf.fill(buf.color(209, 209, 209))
        }
        buf.rect(0, 0, 11, 11);

        s._1 match {
          case "1" =>
            strainResistant match {
              case 'R' =>

                buf.stroke(buf.color(255, 79, 0))
                buf.fill(buf.color(255, 79, 0))

              case 'S' =>
                buf.fill(buf.color(0, 0, 255))
                buf.stroke(buf.color(0, 0, 255))
              case _ =>
                buf.fill(buf.color(100, 100, 100))
                buf.stroke(buf.color(100, 100, 100))
            }

            buf.rect(0, 0, 11, 11)
          case "0" =>
            buf.fill(buf.color(255, 255, 255, 0))
            buf.stroke(230)
            buf.rect(0, 0, 11, 11)
          case _ =>
            buf.fill(buf.color(255, 0, 0))
            buf.stroke(0)
            buf.rect(0, 0, 11, 11)
        }
        if (idx % 10 == 0)
          buf.translate(12, 0)
        idx += 1
        buf.translate(12, 0)
      }
      buf.popMatrix()
    }

  }

}

