package com.ky

import scala.collection.mutable.ListBuffer
import scala.io.Source

object ExpandLyric {

  def main(args: Array[String]) = {
    val output = expand(getLrcAsSeq())
    output.foreach { x => println(x) }
  }

  val tmarker = """(\[\d\d:\d\d\.\d\d\])""".r

  def getLrcAsSeq(): Seq[String] = {
    val source = Source.fromURL(getClass.getResource("/two_tigers.txt"), "utf-8")
    //val res: Array[String] = sampleInput.split("\n")
    val lines = try source.getLines mkString "\n" finally source.close()
    return lines.split("\n")
  }

  def containTimeMarker(aline: String): Boolean = {
    aline match {
      case tmarker.unanchored(m) => return true
      case _                     => return false
    }
  }

  def getMarksLyricList(aline: String): List[String] = {
    val all = (tmarker findAllIn aline).toList
    val lyric = aline.substring(aline.lastIndexOf(']') + 1)
    return all map (_ + lyric)
  }

  def expand(input: Seq[String]): Seq[String] = {
    val all = new ListBuffer[String]
    val lyrics = new ListBuffer[String]

    input.foreach { line =>
      if (!containTimeMarker(line)) {
        all += line
      } else {
        val mAndC = getMarksLyricList(line)
        lyrics ++= mAndC
      }
    }

    val sorted = lyrics sortWith (_ < _) 
    all ++= sorted
    return all.toList
  }

}