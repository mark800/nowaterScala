package com.ky

import scala.collection.mutable.ListBuffer
import scala.io.Source
//所有的return 可以完全省略掉
object ExpandLyric {

  def main(args: Array[String]) {
    val output = expand(getLrcAsSeq())
    output.foreach { x => println(x) }
  }

  val tmarker = """(\[\d\d:\d\d\.\d\d\])""".r

  def getLrcAsSeq(): Seq[String] = {
    val source = Source.fromURL(getClass.getResource("/two_tigers.txt"), "utf-8")
    //val res: Array[String] = sampleInput.split("\n")
    val lines = try source.getLines mkString "\n" finally source.close()//这个地方为什么 mkString, 下面又split了？ 直接 source.getLines？
    //如果用try finally 的话 考虑下Try
    return lines.split("\n")
  }

  def containTimeMarker(aline: String): Boolean = aline match {
    case tmarker.unanchored(m) => true
    case _                     => false
  }

  def getMarksLyricList(aline: String): List[String] = {
    val all = (tmarker findAllIn aline).toList
    val lyric = aline.substring(aline.lastIndexOf(']') + 1)
    all map (_ + lyric)
  }

  def expand(input: Seq[String]): Seq[String] = {//可用 fold/reduce  + Tuple2 来实现
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

    (all ++= sorted).toList
  }

}
