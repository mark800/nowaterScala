package com.ky

import org.scalatest._
import scala.collection.mutable.ListBuffer

class TestExpandLyric extends FlatSpec with Matchers {

  "第一个时间标志之前的内容" should "和输入的内容完全相同" in {
    val inputSeq = ExpandLyric.getLrcAsSeq()
    val outputSeq = ExpandLyric.expand(inputSeq)
    def LoopUtilTimeMarker: Unit = {
      (0 until (inputSeq.size min outputSeq.size)).foreach { i =>
        val containsTM = ExpandLyric.containTimeMarker(inputSeq(i))
        if (!containsTM) {
          assert(inputSeq(i) == outputSeq(i))
        } else {
          return
        }
      }
    }
    LoopUtilTimeMarker
  }

  "结果的每一行" should "最多含有一个时间标志" in {
    val inputSeq = ExpandLyric.getLrcAsSeq()
    val outputSeq = ExpandLyric.expand(inputSeq)
    outputSeq.foreach { line =>
      val found = (ExpandLyric.tmarker findAllIn line).toList.size
      assert(found < 2)
    }
  }
  
    "结果的时间标志总数" should "和输入的时间标志总数相同" in {
    val inputSeq = ExpandLyric.getLrcAsSeq()
    val outputSeq = ExpandLyric.expand(inputSeq)
    
    var inputNum = 0
    inputSeq.foreach { line =>
      val found = (ExpandLyric.tmarker findAllIn line).toList.size
      inputNum += found
    }
    
    var outputNum = 0
    outputSeq.foreach { line =>
      val found = (ExpandLyric.tmarker findAllIn line).toList.size
      outputNum += found
    }
    
    assert(inputNum == outputNum)
  }

  "所有行的时间标志" should "严格按照时间升序排列" in {
    val inputSeq = ExpandLyric.getLrcAsSeq()
    val outputSeq = ExpandLyric.expand(inputSeq)
    val buf = new ListBuffer[String]
    outputSeq.foreach { line =>
      if (ExpandLyric.containTimeMarker(line)) {
        val tm = ExpandLyric.tmarker findFirstIn line getOrElse "wrong"
        buf += tm
      }
    }
    val tArray = buf.toArray
    (0 until (tArray.size - 1)).foreach { i => assert(tArray(i) < tArray(i + 1)) }
  }

}