package peacock.support

import atk.util.Tool

import java.io.File
import java.io.PrintWriter


object CCLM2matrix extends Tool {
  
  override val version="""cclm2matrix converts CCLM files to matrix files for visualization purposes.
    20141104b: Added command-line option to set sort column manually
    20141104a: Initial version information, see repository for older information.
  
  """

  
  
  case class Config(val minimum: Int = 0, val input: File = null, val outputPrefix: String = null,val sortColumn:Int=3)
  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar cclm2matrix") {
      opt[String]('o', "output") required () action { (x, c) => c.copy(outputPrefix = x) } text ("File prefix for the output files.")
      opt[File]('i', "input") required () action { (x, c) => c.copy(input = x) } text ("Input file in CCLM format")
      opt[Int]("minimum") action { (x, c) => c.copy(minimum = x) } text ("Minimum number of times a column needs to be present to be included")
      opt[Int]("sort") action { (x, c) => c.copy(sortColumn = x) } text ("Column index to use for sorting (zero based), columns are comma separated. (Default=3)")
    }
    parser.parse(args, Config()) map { config =>
      cclm(config)
    }
  }
  def cclm(config: Config) {
    val map = tMap(tLines(config.input)).mapValues(f=>if(f.size==0)"none" else f)

  
    println("mapping: " + map)
    val set1 = map.map(f => f._2.split(";").toList).toList.flatten.toSet.toList //.filter(f => genes.contains(f.split(":")(0)))
    val set = if (set1(0).split(",").size > config.sortColumn) {
      set1.sortBy(f => {
        val split = f.split(",")
        //    	   assume(split.size>3,"Not enough field: "+split.toList)
        if (split.size > config.sortColumn)
          split(config.sortColumn).toInt
        else -1
      })
    } else set1.sortBy(identity)
    
    val pw = new PrintWriter(config.outputPrefix + ".ap.matrix")
    pw.println(generatorInfo)
   
    pw.println("# column count = "+set.size)
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
    val mat = tLines(config.outputPrefix + ".ap.matrix").map(l => l.split("\t").toList).transpose
    val filtered = mat.filter(row => {
      println("R: " + row.take(10))
      row(0).equals("$$") || row.drop(1).map(_.toInt).fold(0)((a, b) => a + b) >= config.minimum
    }).transpose
    val pwx = new PrintWriter(config.outputPrefix + ".ap.filtered.matrix")
    pwx.println(generatorInfo)
    pwx.println("# row count = "+filtered.size)
    pwx.println("# column count = "+filtered(0).size)
    pwx.println(filtered.map(line => line.mkString("\t")).mkString("\n"))
    pwx.close

  }

}