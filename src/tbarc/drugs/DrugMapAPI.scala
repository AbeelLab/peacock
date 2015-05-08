package tbarc.drugs

import scala.io.Source
import java.io.File
import atk.util.Tool



class DrugMapAPI(file: File)extends Tool {

  
  
  private val rawInput = Source.fromFile(file).getLines.toList;
  val drugData = tMap(tLines(file))//rawInput.filterNot(_.startsWith("#"))
  println("Drug data: "+drugData)
  
  if(!drugData.contains("$$"))println("WARNING: The drug data file you provided does not contain a header row that is identified with $$. The $$ sign is used to uniformly identify the header row to identify access to columns.")


  val drugMapIn=drugData.mapValues(_.split("\t"))

  def countRSC(drug: String): Map[Char,Int] = {
	 val specific=drugMap.mapValues(f=>f.getOrElse(drug, '-'))
	 val countRS=specific.groupBy(f=>f._2).mapValues(f=>f.size);
	 countRS
  }

 


  private def process(id: String, profile: Array[String]) = {
    val zipped = profile.zip(drugIdentifiers)
    assume(profile.length==drugIdentifiers.length)
//    println("zip: "+zipped.toList.mkString("\n"))
    
    
    val resistance=zipped.map(f=>(f._2-> (f._1(0) match{
      case '0' => 'S'
      case '1' => 'R'
      case _ => 'U'
    }))).toList.toMap
    

    println(id+"\t"+resistance)
    id -> resistance
  }

  val drugIdentifiers = if(drugMapIn.contains("$$")) (drugMapIn.getOrElse("$$", null)).toList else List.empty

  val drugMap = for ((id, profile) <- drugMapIn) yield {
    process(id, profile)
  }

  val drugIndexMap = drugIdentifiers.zipWithIndex.toMap

  val sortedDrugLabels = drugIdentifiers.toSet.toList.sortWith(_.toLowerCase < _.toLowerCase())//.sortWith(Drug.sorter)
  
  println("# Sorted Drugs:")
  sortedDrugLabels.map(f=>println(f))
  

}