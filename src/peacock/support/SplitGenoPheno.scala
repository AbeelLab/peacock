package peacock.support

import java.io.File
import atk.util.Tool
import java.io.PrintWriter
import scala.Array.canBuildFrom
import java.text.NumberFormat
import java.util.Locale
import atk.compbio.tree.Tree
import scala.collection.JavaConversions._

object SplitGenoPheno extends Tool {
  case class Config(val tree: File = null, val input: File = null, val outputPrefix: String = "")

  override val description="""
    	Tool to prepare geno-phenotype information from Chris script for visualization and summarize the information in  tables.
    """
  
  override val version="""
    pre 2015:    Initial version used in TB-ARC projects
    2015/01/08:  More detailed sanity checking information
    """
  
  def main(args: Array[String]): Unit = {

    //    split(new File("v:/TB-ARC/KRITH_extended/test_run.snps_to_known_muts.txt"), "v:/TB-ARC/KRITH_extended/test_run.snps_to_known_muts")
    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar prep-genotype-phenotype") {
      opt[File]('i', "input") required () action { (x, c) => c.copy(input = x) } text ("Input file, *.snps_to_known_muts.txt")
      opt[File]('t', "tree") required () action { (x, c) => c.copy(tree = x) } text ("Tree file, this is used to determine which strains to include in summary table. ")
      opt[String]('o', "output") action { (x, c) => c.copy(outputPrefix = x) } text ("Output prefix, default=\"\"")
    }

    parser.parse(args, Config()).map { config =>
      split(config.input, config.outputPrefix, config)
    }
  }

  def split(input: File, outputPrefix: String, config: Config) = {
    val lines = tLines(input, false, true).toList
    val pwG = new PrintWriter(outputPrefix + "genotypes.mat")
    val pwP = new PrintWriter(outputPrefix + "phenotypes.mat")
    pwG.println(generatorInfo)
    pwP.println(generatorInfo)
    pwG.println("# input=" + input)
    pwP.println("# input=" + input)
    pwG.println("# tree="+ config.tree.getAbsolutePath())
    pwP.println("# tree="+ config.tree.getAbsolutePath())
    pwG.println(lines(0).replaceAll("#genome", "\\$\\$"))
    pwP.println(lines(0).replaceAll("#genome", "\\$\\$"))
    val tree = new Tree(config.tree.toString())
    println("leaves: " + tree.getLeaves(tree.root))
    val leafList = tree.getLeaves(tree.root).map(l => l.getName()).toList.filterNot(_.equals("reference"))

    println(leafList.size)
    val values = lines.drop(1).filter(line => leafList.contains(line.split("\t")(0)))
    values.map(f => {
      val arr = f.split("\t")
      val arx = arr.drop(1).map(f => {
        f.map(g => g match {
          case '0' => 'S'
          case '1' => 'R'
          case '-' => 'U'

        })
      })
      pwG.println(arr(0) + "\t" + arx.map(f => f(0)).mkString("\t"))
      pwP.println(arr(0) + "\t" + arx.map(f => f(1)).mkString("\t"))
    })

    pwG.close()
    pwP.close

    /*
     * Make Campbell lite table
     */
    val pwX = new PrintWriter(outputPrefix + "genopheno.summary.txt")
    pwX.println(generatorInfo)
    pwX.println("#")
    pwX.println("# input=" + input)
    pwX.println("# tree="+ config.tree.getAbsolutePath())
    pwX.println("#")
    pwX.println("# Genotype/phenotype")
    pwX.println("# drug \tRR\tSR\tRS\tSS\tsensitivity\tspecificity\tF\tsanity\tmissing/ambiguous debug\t0-\t1-\t-0\t-1\t--")
    pwX.println("#      \tTP\tFN\tFP\tTN")
    val list = List("11", "01", "10", "00","0-","1-","-0","-1","--")
//    pwX.println("#      \t"+list.mkString("\t"))
    val matrix = lines.map(l => l.split("\t").toList).filter(l => l(0).equals("#genome") || leafList.contains(l(0))).transpose

    val gnumbers = matrix.filter(_(0).startsWith("#"))(0).drop(1)
    println("GNumbers: " + gnumbers)
    val pwY = new PrintWriter(outputPrefix + "genopheno.strain_details.txt")

    for (drugLine <- matrix.filterNot(_(0).startsWith("#"))) {
      pwY.println("## "+drugLine(0))
      pwY.println("##--------------------")
      val sum = drugLine.drop(1).groupBy(identity).mapValues(_.size)
      val values = list.map(sum.getOrElse(_, 0))
      //      val sen = values(0) / (values(0).toDouble + values(1))
      //      val spec = values(3) / (values(2).toDouble + values(3))
      val tp = values(0).toDouble
      val fn = values(1).toDouble
      val fp = values(2).toDouble
      val tn = values(3).toDouble
      val missing=values.drop(4).sum
      val sen = tp / (tp + fn)
      val spec = tn / (fp + tn)
      //      val acc = (tp + tn) / (tp + tn + fn + fp)
      val F = (2 * tp) / (2 * tp + fp + fn)
      val sanity = tp + fp + fn + tn
      pwX.println(drugLine(0) + "\t" + values.take(4).mkString("\t") + "\t" + nfP.format(sen) + "\t" + nfP.format(spec) + "\t" + nfP.format(F) + "\t" + sanity+"\t"+missing+"\t"+values.drop(4).mkString("\t"))

      val mapping = gnumbers.zip(drugLine.drop(1)).groupBy(_._2)
      mapping.map(f => {
        val matchingNumbers=f._2.map(_._1)
        pwY.println(drugLine(0) + "\t" + f._1 + "\t"+ matchingNumbers.size+"\t"+matchingNumbers.mkString(",") )

      })

      

    }
    pwY.close
    pwX.close

  }

}
