package peacock.vignets

import tbarc.drugs.DrugMapAPI
import processing.core.PGraphics
import java.io.File

class MetaBinaryVignets(matrixFile: File, drug: String, drugs: DrugMapAPI) extends BinaryVignets(matrixFile) {

  override def image(buf: PGraphics, key: String) {
    buf.pushMatrix()
    //      val buf: PGraphics = applet.createGraphics(x, y); //createGraphics(890, yPixels);
    val l = matrix.getOrElse(key, List("Missing value: " + key))

    buf.translate(4, 0)
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
      //      buf.rect(0, 0, x, y);
      buf.rect(0, 0, 11, 11)
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
      if (idx % 10 == 0) {
        buf.translate(12, 0)
      }
      idx += 1

      buf.translate(12, 0)
    }
    //        buf.fill(buf.color(255,255,255,255))
    //      buf.stroke(buf.color(255,255,255,255))
    buf.popMatrix()
    //      buf.endDraw()
    //      buf.get(0, 0, buf.width, buf.height);
  }

}