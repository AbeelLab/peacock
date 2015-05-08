package peacock.figures

import atk.util.Tool
import java.io.PrintWriter

import java.io.File
import atk.io.PatternFileFilter

import peacock.core.TreeViz

import peacock.vignets.BinaryVignets
import peacock.core.LabelGenerator
import scala.Array.canBuildFrom
import atk.compbio.tree.Tree

import peacock.core.TableViz

object LorikeetFigure extends Tool {
  case class Config(val input: File = null, val outputPrefix: String = null, val pgg: File = null, val lineage: File = null, val tree: String = null, val manhattan: String = null)

  def main(args: Array[String]): Unit = {
    val spacersToUse = 43
    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar spoligotype") {
      opt[File]('i', "input") required () action { (x, c) => c.copy(input = x) } text ("Spoligotype matrix file.")
      opt[String]('o', "output") required () action { (x, c) => c.copy(outputPrefix = x) } text ("File prefix for the output files.")

      opt[String]('t', "tree") required () action { (x, c) => c.copy(tree = x) } text ("File containing the phylogenetic tree in NWK format.")

      opt[File]("pgg") action { (x, c) => c.copy(pgg = x) } text ("File containing principal genetic group information.")

      opt[File]("lineage") action { (x, c) => c.copy(lineage = x) } text ("File containing lineage information, by default this is generated from the spoligotype.")
  
    }
    parser.parse(args, Config()) map { config =>

      val spoligoTypeMatrix = config.input


      val labels = new LabelGenerator

      val vignets = List(new BinaryVignets(spoligoTypeMatrix))
      if (config.tree == null) {
        val sorting = tMap(tLines(spoligoTypeMatrix)).keySet.filter(_.equals("$$")).toList.sortBy(identity)

        TableViz.make(sorting, labels, vignets, output = config.outputPrefix + ".peacock.spoligotype.")
      } else {
        val tree = new Tree(config.tree)

        TreeViz.make(tree, treeWidth = 800, labels = labels, vignets = vignets, exportPrefix = config.outputPrefix + ".peacock.spoligotype.")

      }

    }

  }

}