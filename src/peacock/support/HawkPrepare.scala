package peacock.support

import java.io.File

object HawkPrepare {

  case class Config(input: File = new File("reduced_vcfs"), tree: File = null)
  def main(args: Array[String]): Unit = {
    
    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar prep-hawk [options]") {
      opt[File]('t', "tree") required () action { (x, c) => c.copy(tree = x) } text ("Input directory where to search for VCF files. Default=reduced_vcfs")
      opt[File]('i', "input") action { (x, c) => c.copy(input = x) } text ("Folder that contains VCF files")

    }
    parser.parse(args, Config()) map { config =>
      CreateBuffers.execute(config.input, config.tree)
    }

  }

}