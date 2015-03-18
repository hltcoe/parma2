// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference.topics

import scala.collection.mutable
import scala.collection.immutable

import java.io._

object Runner {

  val usage = """
    Usage: lda [--num-topics num] [--inference variational|gibbs] [--input_format pre|raw] corpus_path
  """

  def main(args: Array[String]) {
    if (args.length == 0) println(usage)
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "--num-topics"   :: value :: tail =>
          nextOption(map ++ Map('num_topics -> value.toInt), tail)
        case "--inference"    :: value :: tail =>
          nextOption(map ++ Map('inference -> value.toString), tail)
        case "--input-format" :: value :: tail =>
          nextOption(map ++ Map('format -> value.toString), tail)
        case "--vocab" :: value :: tail =>
          nextOption(map ++ Map('vocab -> value.toString), tail)
        case string :: opt2 :: tail if isSwitch(opt2) =>
          nextOption(map ++ Map('infile -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
        case option :: tail => println("Unknown option "+option) 
          sys.exit(1)
      }
    }

    val options = nextOption(Map(), arglist)

    val N = (options get 'num_topics).getOrElse(8)
    val alg = (options get 'inference).getOrElse(new String("variational"))
    val pathname = (options get 'infile).getOrElse(new String("data/ap/ap.dat"))
    val format = (options get 'format).getOrElse(new String("pre"))
    val vocab = (options get 'vocab).getOrElse(new String("data/ap/vocab.txt"))

    println("Input path: " + pathname)
    println("Format: " + format)
    println("Inference algorithm: " + alg)
    println("Number of topics: " + N)

    val path = new File(pathname.asInstanceOf[String])

    println("path as string: " + pathname.asInstanceOf[String])

    var docs = scala.collection.mutable.ArrayBuffer.empty[Document]
    format match {
      case "pre" => {
        val vocab_path = new File(vocab.asInstanceOf[String])
        docs = Document.fromPreprocessedFile(path, vocab_path)
      }
      case "raw" => {
        val iter = new DirectoryIterator(path)
        for(f <- iter.filter(_.isFile).toList.sorted) {
          docs += Document.fromRawFile(f)
        }
      }
    }

    println(docs.size + " documents loaded")
    println(Document.getVocabSize + " vocabulary words")

    // Train the model
    val trainer = new LDATrainer(N.asInstanceOf[Int], Document.getVocabSize, docs)

//    trainer.runEM

    // Print some info on the model
    var k = 0
    while(k < trainer.model.num_topics) {
      println(trainer.stats.class_total(k))
      k += 1
    }

    // Save the vocab
    println("indices before serialization")
    println(Document.getIndex("percent") + ", " + Document.getWord(Document.getIndex("percent")))
    println(Document.getIndex("year") + ", " + Document.getWord(Document.getIndex("year")))
    println(Document.getIndex("government") + ", " + Document.getWord(Document.getIndex("government")))

    Document.writeWordMap("myvocab.txt")

    // Read the vocab
    Document.wordMap = Map.empty
    Document.keyMap  = Map.empty

    Document.readWordMap("myvocab.txt")

    println("indices after serialization")
    println(Document.getIndex("percent") + ", " + Document.getWord(Document.getIndex("percent")))
    println(Document.getIndex("year") + ", " + Document.getWord(Document.getIndex("year")))
    println(Document.getIndex("government") + ", " + Document.getWord(Document.getIndex("government")))

    // Serialize the result
    println("serializing...")
    val oos = new ObjectOutputStream(new FileOutputStream("lda.model"))
    oos.writeObject(trainer)
    oos.close

    // Read it back
    println("reading it back...")
    val ois = new ObjectInputStream(new FileInputStream("lda.model"))
    val maybe_trainer = Some(ois.readObject.asInstanceOf[LDATrainer]).get
    ois.close

    // Print some info on the model
    k = 0
    while(k < maybe_trainer.model.num_topics) {
      println(maybe_trainer.stats.class_total(k))
      k += 1
    }
  }
}

