package peacock.support

import java.io.File
import atk.io.PatternFileFilter
import atk.util.Tool
import atk.compbio.vcf.VCFLine
import java.io.PrintWriter

object CountPerTypePrep extends Tool {
  val defaultPattern = ".*.vcf"
  val keys = List("SingleSubstitution", "SingleDeletion", "SingleInsertion", "LongSubstitution", "LongDeletion", "LongInsertion")

  case class Config(val pattern: String = defaultPattern, input: File = null, output: File = null)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar prep-cpt") {
      opt[String]('p', "pattern") action { (x, c) => c.copy(pattern = x) } text ("File pattern to search and process. [default=" + defaultPattern + "]")
      opt[File]('i', "input") required () action { (x, c) => c.copy(input = x) } text ("Input directory where to search for VCF files.")
      opt[File]('o', "output") action { (x, c) => c.copy(output = x) } text ("Output prefix which will precede every output file.")

    }
    parser.parse(args, Config()) map { config =>
    run(config)
    }
  }
  def run(config: Config) {
    val inData = (for (file <- config.input.listFiles(new PatternFileFilter(config.pattern))) yield {
      println("Loading: " + file + "\t" + file.length())
      val vcfLines = tLinesIterator(file).map(p => new VCFLine(p)).filter(line => line.pass)

      val types = vcfLines.toList.map(p => p.variation.strType)

      val cm = types.groupBy(identity).map { case (x, y) => (x, y.size) }.toList.sortBy { _._1 }

      println(cm)

      (file.toString(), cm.toMap)
    }).toMap
    println("indata: " + inData.toList)

    /*
      *   Output table
      */
    val pw = new PrintWriter(config.output + ".cpt.table.txt")
    pw.println(generatorInfo)
    pw.println("## G\t" + keys.mkString("\t"))
    inData.keysIterator.toList.sortBy(identity).map(g => {
      pw.println(g + "\t" + (keys.map(k => inData.getOrElse(g, null).getOrElse(k, 0))).mkString("\t"))
    })
    pw.close

  }

}