package peacock.figures

import peacock.vignets.LabeledHeatMapVignet
import atk.util.Tool
import peacock.core.TableViz
import java.io.File
import atk.compbio.tree.Tree
import peacock.core.TreeViz
import tbarc.drugs.DrugMapAPI
import peacock.vignets.MetaBinaryVignets
import peacock.core.LabelGenerator


object MICfigure extends Tool {
  def remap(map: Map[String, String], labels: List[String]) = {
    map.mapValues(f => {

     labels.indexOf(f)
     
    })
  }
  case class Config(

    val outputPrefix: String = null,
    val pgg: File = null,
    val lineage: File = null,
    val tree:File =null,
    val dst: File = null,
    val drug: String = null,
    val mic: File = null,
    val variant: File = null)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("java -jar peacock.jar mic-table") {
      /* Output prefix */
      opt[String]('o', "output") required () action { (x, c) => c.copy(outputPrefix = x) } text ("File prefix for the output files.")

      opt[File]('t', "tree") action { (x, c) => c.copy(tree = x) } text ("File containing a tree. If missing, will draw a table.")
      
      /* Labeling information */
      opt[File]("pgg") required () action { (x, c) => c.copy(pgg = x) } text ("File containing principal genetic group information.")
      opt[File]("lineage") required () action { (x, c) => c.copy(lineage = x) } text ("File containing lineage information, by default this is generated from the spoligotype.")
      
      opt[File]("dst") required () action { (x, c) => c.copy(dst = x) } text ("File containing binary (R/S) DST information.")
      opt[String]("drug") required () action { (x, c) => c.copy(drug = x) } text ("Drug identifier, e.g. ofloxacin")
      opt[File]("mic") required () action { (x, c) => c.copy(mic = x) } text ("File containing MIC information. GNumber<tab>MIC")
      opt[File]("variant") required () action { (x, c) => c.copy(variant = x) } text ("File containing variant AP matrix")

    }

    parser.parse(args, Config()) map { config =>
      val labels = List("-","0.03","0.06","0.12","0.125", "0.25", "0.5","0.6", "1", ">1","1.2", "2", ">2","2.5", "4",">4","5", "8",">8","10", "16",">16","20", "32", ">32","40",">40")
      val map = remap(tMap(tLines(config.mic)), labels)
      val reverseList = map.filterNot(p =>p._1.equals("$$")).toList.sortBy(-_._2).map(_._1)
      val mic = new LabeledHeatMapVignet("MIC", labels, map)
    val drugs = new DrugMapAPI(config.dst)
      val oflox = drugs.drugIdentifiers.filter(_.contains(config.drug))
      println("XF: " + oflox)


      for (ofx <- oflox) {

        val dst = new MetaBinaryVignets(config.variant, ofx, drugs)

        val labels = new LabelGenerator

        if(config.tree==null)
        	TableViz.make(reverseList, labels = labels, vignets = List(mic, dst), output = config.outputPrefix)
        else {
          val tree=new Tree(config.tree.toString())
          TreeViz.make(tree, labels = labels, vignets = List(mic, dst), exportPrefix = config.outputPrefix)
        }
      }
    }

  }

}