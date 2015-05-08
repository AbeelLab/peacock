package peacock.vignets

import peacock.core.VignetMaker
import scala.io.Source
import processing.core.PApplet
import processing.core.PImage
import processing.core.PGraphics
import atk.util.Lines
import atk.util.Tool._
import peacock.core.FreeFormAddition
import processing.core.PConstants
import java.io.File

class PhenotypeGenotypeVignets(phenotypeMatrix: File, genotypeMatrix: File, ordering: List[(String, String)] = null, val radial: Boolean = false) extends BinaryVignets(phenotypeMatrix, {

  if (ordering != null)
    ordering
  else {
    val order = List("rifampicin", "rifabutin", "isoniazid", "pyrazinamide", "niacin", "nicotinamide", "ethambutol", "streptomycin", "kanamycin", "amikacin", "capreomycin", "ofloxacin", "moxifloxacin", "ciprofloxacin", "cycloserine", "ethionamide", "prop_4aminosalicylic_acid")

    assume(phenotypeMatrix.exists(), "Phenotype file does not exist: " + phenotypeMatrix)
    //          assume(config.geno.exists(), "Genotype file does not exist: " + config.geno)

    val header = tMap(tLines(phenotypeMatrix)).getOrElse("$$", null)
    assume(header != null)
    val orderWithLabels = header.split("\t").toList.map(f => {
      (order.indexOf(f.split("\\$")(0)), f)
    })
    println("Sorting: " + orderWithLabels)
    val tmp = orderWithLabels.sortBy(_._1).map(_._2)
    tmp.zip(tmp)
  }
}) {
  assume(phenotypeMatrix != null)

  val genotypeMap = if (genotypeMatrix != null) {
    val arrs = tLines(genotypeMatrix).map(_.split("\t")).toList
    val keys = arrs(0)
    arrs.map(arr => {
      arr(0) -> keys.zip(arr).toMap

    }).toMap

  } else Map.empty[String, Map[String, String]]

  val phenotypeMap = if (phenotypeMatrix != null) {
    val arrs = tLines(phenotypeMatrix).map(_.split("\t")).toList
    val keys = arrs(0)
    arrs.map(arr => {
      arr(0) -> keys.zip(arr).toMap

    }).toMap

  } else Map.empty[String, Map[String, String]]

  val pgLegend = new FreeFormAddition {
    override def drawFreeForm(g: PGraphics) = {
      g.translate(50, 10)
      val sl = 25
      g.stroke(0)
      g.fill(255)
      g.rect(50, 0, boxSize, boxSize)
      g.rect(50, sl, boxSize, boxSize)
      g.fill(0)
      g.textAlign(PConstants.LEFT);
      if (!genotypeMap.isEmpty) {
        g.triangle(50, 0, 50 + boxSize, 0, 50, boxSize)

        g.triangle(50 + boxSize, sl, 50 + boxSize, boxSize + sl, 50, boxSize + sl)

        g.text("Phenotype", 65, 10)
        g.text("Genotype", 65, sl + 10)
      } else {
        g.rect(50, 0, boxSize, boxSize)
        g.text("Phenotype", 65, 10)
      }

      g.stroke(g.color(255, 79, 0))
      g.fill(g.color(255, 79, 0))
      g.rect(350, 0, boxSize, boxSize)

      g.fill(g.color(0, 0, 255))
      g.stroke(g.color(0, 0, 255))
      g.rect(350, sl, boxSize, boxSize)

      g.fill(0)
      g.stroke(0)
      g.text("Resistant", 365, 10)
      g.text("Susceptible", 365, sl + 10)

      g.fill(g.color(200, 200, 200))
      g.stroke(g.color(150, 150, 150))
      g.rect(550, 0, boxSize, boxSize)

      g.fill(g.color(255, 255, 255))
      g.stroke(g.color(200, 200, 200))
      g.rect(550, sl, boxSize, boxSize)

      g.fill(0)
      g.stroke(0)
      g.text("Ambiguous genotype", 565, 10)
      g.text("Missing data", 565, sl + 10)

    }
  }

  override def header(buf: PGraphics) {
    assume(x > 0, "X is zero")
    assume(headerHeight > 0, "headerHeight is zero")
    //    val buf: PGraphics = applet.createGraphics(x, headerHeight); //createGraphics(890, yPixels);
    //    buf.beginDraw()
    buf.pushMatrix()
    buf.fill(0)
    buf.stroke(0)
    buf.translate(-2, headerHeight)
    buf.rotate(-PConstants.HALF_PI)
    var idx = 0
    for (l <- useOrder) {
      buf.text(l._2, 0, 12);
      buf.translate(0, 12)
      idx += 1
      if (idx % 3 == 0)
        buf.translate(0, 4)
    }
    buf.popMatrix()
    //    buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height);
  }

  override def y() = { 15 }
  override def x() = { useOrder.size * boxSize + 100 }

  private val boxSize = y - 3

  override def image(buf: PGraphics, key: String) {
    //    val buf = applet.createGraphics(x, y); //createGraphics(890, yPixels);

    //    buf.beginDraw()
    buf.pushMatrix()
    buf.translate(0, 1)
    buf.fill(0)

    var idx = 0
    var lastLine = 1

    var agree = 0
    var blank = 0
    var disagree = 0
    var ambiguous = 0
    val genotype = genotypeMap.getOrElse(key, Map.empty[String, String])
    println("GT: " + genotype)
    val phenotype = phenotypeMap.getOrElse(key, Map.empty[String, String])
    println("PT: " + phenotype)

    for (drugLabel <- useOrder) {

      val c = phenotype.getOrElse(drugLabel._1, "U")(0)

      c match {
        case 'R' =>
          buf.stroke(buf.color(255, 79, 0))
          buf.fill(buf.color(255, 79, 0))

        case 'S' =>
          buf.fill(buf.color(0, 0, 255))
          buf.stroke(buf.color(0, 0, 255))

        case _ =>
          buf.fill(buf.color(255, 255, 255))
          buf.stroke(buf.color(200, 200, 200))

      }
      if (genotypeMap.isEmpty) {
        if (radial)
          buf.ellipse(idx * boxSize*2, 0+boxSize, boxSize, boxSize)
        else
          buf.rect(idx * boxSize, 0, boxSize, boxSize)

      } else {
        if (radial)
          buf.ellipse(idx * boxSize*2, 0+boxSize, boxSize, boxSize)
        else
          buf.triangle(idx * boxSize, 0, idx * boxSize + boxSize, 0, idx * boxSize, boxSize)
      }

      // println("G="+gNumber)
      if (!genotypeMap.isEmpty) {
        //        println("\tDG")
        genotype.getOrElse(drugLabel._1, "U")(0) match {
          case 'R' =>
            buf.stroke(buf.color(255, 79, 0))
            buf.fill(buf.color(255, 79, 0))
            if (c == 'R')
              agree += 1
            if (c == 'S')
              disagree += 1

          case 'S' =>
            buf.fill(buf.color(0, 0, 255))
            buf.stroke(buf.color(0, 0, 255))
            if (c == 'R')
              disagree += 1
            if (c == 'S')
              agree += 1

          case 'U' =>
            buf.fill(buf.color(200, 200, 200))
            buf.stroke(buf.color(150, 150, 150))
            ambiguous += 1
          case _ =>
            println("\t\toops")
            buf.fill(buf.color(255, 255, 255))
            buf.stroke(buf.color(200, 200, 200))
            blank += 1
        }
        if(radial)
          buf.ellipse(idx * boxSize*2, 0+boxSize, boxSize/2-2, boxSize/2-2)
        else
        buf.triangle(idx * boxSize + boxSize, 0, idx * boxSize + boxSize, boxSize, idx * boxSize, boxSize)
        //      buf.rect(idx * yPixels, yPixels / 2, yPixels, yPixels / 2)
      }
      idx += 1
      if (idx % 3 == 0)
        buf.translate(4, 0)
    }
    //    pwUD.println()
    //    buf.translate(5, 0)
    //    println("extra: " + GNumbers.singleG(gNumber) + "\t" + extraStuff._2.getOrElse(GNumbers.singleG(gNumber), ""))
    //    for (x <- extraStuff._2.getOrElse(GNumbers.singleG(gNumber), "").split("\t")) {
    //
    //      buf.stroke(color(200, 200, 200))
    //      x match {
    //        case "0" =>
    //          buf.fill(color(255, 255, 255))
    //        case "1" =>
    //          buf.fill(color(0, 0, 0))
    //        case "-" =>
    //          buf.fill(color(100, 100, 100))
    //        case _ =>
    //          buf.fill(color(255, 255, 255))
    //      }
    //
    //      buf.rect(idx * yPixels, 0, yPixels, yPixels)
    //      idx += 1
    //      buf.translate(2, 0)
    //
    //    }
    if (disagree >= 3) {
      buf.fill(buf.color(255, 0, 0))
      buf.stroke(buf.color(255, 0, 0))
    } else {
      buf.fill(buf.color(100, 100, 100))
      buf.stroke(buf.color(100, 100, 100))
    }
    buf.translate(10, 0)
    //    pwDiscordant.println(GNumbers.singleG(gNumber) + "\t" + mName(GNumbers.singleG(gNumber)) + "\t" + agree + "\t" + disagree + "\t" + ambiguous + "\t" + blank)
    buf.text(agree + " " + disagree + " " + ambiguous + " " + blank, idx * boxSize, boxSize)
    //    buf.endDraw()
    //    buf.get(0, 0, buf.width, buf.height)
    //    buf.get(0, 0, buf.width, buf.height);
    buf.popMatrix()

  }

  /*def createVignet(profile: Map[Drug, Char], gNumber: String): PImage = {
    val buf = createGraphics(labelWidth, yPixels); //createGraphics(890, yPixels);

    buf.beginDraw()

    buf.fill(0)

    var idx = 0
    var lastLine = 1
  

    var agree = 0
    var blank = 0
    var disagree = 0
    var ambiguous = 0

    for (dr <- drugs.sortedDrugLabels) {

      buf.translate(2, 0)
      if (dr.line != lastLine) {
        buf.translate(5, 0)
        lastLine = dr.line
      }

      val x = drugs.drugIndexMap.getOrElse(dr, -1)
      assume(x != -1)
      val c = profile.getOrElse(dr, 'U') //profile(x)
//      pwUD.print("\t" + c)
      summaryMaps.getOrElseUpdate(dr, new CountMap[Char]()).count(c)

      c match {
        case 'R' =>
          buf.stroke(color(255, 79, 0))
          buf.fill(color(255, 79, 0))

        case 'S' =>
          buf.fill(color(0, 0, 255))
          buf.stroke(color(0, 0, 255))

        case _ =>
          buf.fill(color(255, 255, 255))
          buf.stroke(color(200, 200, 200))

      }
      if (knownMuts.isEmpty)
        buf.rect(idx * yPixels, 0, yPixels, yPixels)
      else
        buf.triangle(idx * yPixels, 0, idx * yPixels + yPixels, 0, idx * yPixels, yPixels)

      // println("G="+gNumber)
      if (!knownMuts.isEmpty) {
        knownMuts.getOrElse(GNumbers.singleG(gNumber), Map.empty[String, String]).getOrElse(dr.chris3Letter, "Missing") match {
          case "1" | "2" =>
            buf.stroke(color(255, 79, 0))
            buf.fill(color(255, 79, 0))
            if (c == 'R')
              agree += 1
            if (c == 'S')
              disagree += 1

          case "3" | "4" =>
            buf.fill(color(0, 0, 255))
            buf.stroke(color(0, 0, 255))
            if (c == 'R')
              disagree += 1
            if (c == 'S')
              agree += 1

          case "5" | "6" =>
            buf.fill(color(200, 200, 200))
            buf.stroke(color(150, 150, 150))
            ambiguous += 1
          case _ =>
            buf.fill(color(255, 255, 255))
            buf.stroke(color(200, 200, 200))
            blank += 1
        }
        buf.triangle(idx * yPixels + yPixels, 0, idx * yPixels + yPixels, yPixels, idx * yPixels, yPixels)
        //      buf.rect(idx * yPixels, yPixels / 2, yPixels, yPixels / 2)
      }
      idx += 1
    }
//    pwUD.println()
    buf.translate(5, 0)
    println("extra: " + GNumbers.singleG(gNumber) + "\t" + extraStuff._2.getOrElse(GNumbers.singleG(gNumber), ""))
    for (x <- extraStuff._2.getOrElse(GNumbers.singleG(gNumber), "").split("\t")) {

      buf.stroke(color(200, 200, 200))
      x match {
        case "0" =>
          buf.fill(color(255, 255, 255))
        case "1" =>
          buf.fill(color(0, 0, 0))
        case "-" =>
          buf.fill(color(100, 100, 100))
        case _ =>
          buf.fill(color(255, 255, 255))
      }

      buf.rect(idx * yPixels, 0, yPixels, yPixels)
      idx += 1
      buf.translate(2, 0)

    }
    if (disagree >= 3) {
      buf.fill(color(255, 0, 0))
      buf.stroke(color(255, 0, 0))
    } else {
      buf.fill(color(100, 100, 100))
      buf.stroke(color(100, 100, 100))
    }
    buf.translate(10, 0)
    pwDiscordant.println(GNumbers.singleG(gNumber) + "\t"+mName(GNumbers.singleG(gNumber)) + "\t" + agree + "\t" + disagree + "\t" + ambiguous + "\t" + blank)
    buf.text(agree + " " + disagree + " " + ambiguous + " " + blank, idx * yPixels, yPixels)
    buf.endDraw()
    buf.get(0, 0, buf.width, buf.height)
  }*/
}