// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference.topics

import scala.collection.immutable
import scala.collection.mutable

import java.io._

class Document(doc: Map[Int,Int]) {

  val words  = Array.ofDim[Int](doc.size)
  val counts = Array.ofDim[Int](doc.size)
  val length = doc.size
  val total  = doc.values.sum

  var n = 0
  for(key <- doc.keys.toList.sortWith((x,y) => x < y)) {
    words(n)  = key
    counts(n) = doc(key)
    n += 1
  }

  override def toString = {
    val buf = new StringBuilder
    n = 0
    while(n < length) {
      buf ++= (Document.getWord(words(n)) + ":" + counts(n) + " ")
    }
    buf.toString
  }
}

object Document {
  var wordMap : immutable.Map[String,Int] = Map.empty
  var keyMap : immutable.Map[Int,String] = Map.empty

  def hasIndex(w: String) = {
    wordMap contains w
  }

  def getIndex(w: String) : Int = {
    if(wordMap contains w) {
      wordMap(w)
    } else {
      val key = wordMap.keys.size
      wordMap += (w -> key)
      keyMap  += (key -> w)
      key
    }
  }

  def getWord(i: Int) : String = {
    keyMap(i)
  }

  def writeWordMap(filename: String) {
    val fw = new FileWriter(filename)
    for((key,value) <- wordMap) {
      fw.write(key + "\t" + value + "\n")
    }
    fw.close
  }

  def readWordMap(filename: String) {
    for( ln <- io.Source.fromFile(filename).getLines.map(_.trim) ) {
      val tokens = ln.split("\t")
      if(tokens.size != 2) {
        println("malformed line: " + ln)
      }
      wordMap += tokens(0) -> tokens(1).toInt
      keyMap  += tokens(1).toInt -> tokens(0)
    }
  }


  def getVocabSize() : Int = {
    wordMap.size
  }

  def splitLine(line: String) : Array[String] = {
    line.split("\\s+")
  }

  def normalizeAndFilter(words: Array[String]) = {
    var list = mutable.ListBuffer[String]()
    for(w <- words) {
      val norm_w = w.toLowerCase.replaceAll("\\W", "")
      if(norm_w.length > 0) {
        list += norm_w
      }
    }
    list
  }

  def fromRawString(s: String) = {
    val lines = s.split("\n")
    val words = normalizeAndFilter(lines.flatMap(splitLine(_)))
    var accum = mutable.Map[Int, Int]().withDefault(x=>0)
    for(w <- words) {
      accum(Document.getIndex(w)) += 1
    }
    var counts = immutable.Map[Int, Int]()
    for((key,value) <- accum) {
      counts += key -> value
    }
    new Document(counts)
  }

  def fromNormalizedString(s: String) = {
    val words = s.split(" ")
    var accum = mutable.Map[Int, Int]().withDefault(x=>0)
    for(w <- words) {
      accum(Document.getIndex(w)) += 1
    }
    var counts = immutable.Map[Int, Int]()
    for((key,value) <- accum) {
      counts += key -> value
    }
    new Document(counts)
  }

  // token sequence
  def fromRawFile(f: File) = {
    val source = f.getAbsolutePath
    fromRawString(scala.io.Source.fromFile(source).mkString)
  }

  // length type:count type:count ...
  def fromPreprocessedFile(f: File, vocabFile: File) = {

    // Load the dictionary file
    for(line <- scala.io.Source.fromFile(vocabFile.getAbsolutePath).getLines) {
      val trimmed = line.trim
      Document.getIndex(trimmed)
    }

    // Load the document type counts
    val docs = scala.collection.mutable.ArrayBuffer.empty[Document]
    val source = f.getAbsolutePath
    val lines = scala.io.Source.fromFile(source).getLines
    for(line <- lines) {
      val tokens = line.split("\\s+")
      val len = tokens.head
      val cs = tokens.drop(1) // list of type:count pairs
      var counts = immutable.Map[Int,Int]()
      for(c <- cs) {
        val wc = c.split(":")
        counts += wc(0).toInt -> wc(1).toInt
      }
      docs += new Document(counts)
    }
    docs
  }
}
