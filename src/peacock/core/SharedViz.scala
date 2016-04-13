package peacock.core

import java.io.File
import atk.util.Tool

object SharedViz extends Tool {

  def readLineage(lineage: File): Map[String, String] = {
    if (lineage != null)
      tMap(tLines(lineage), keyColumn = 0, valueColumn = 1, limitSplit = false)
    else
      Map.empty[String, String]
  }
}