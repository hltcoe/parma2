// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.experiments.Corpus
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.math.Stats
import edu.jhu.hlt.parma.inference.topics.LDATrainer
import no.uib.cipr.matrix.VectorEntry
import no.uib.cipr.matrix.sparse.SparseVector
import scala.collection.JavaConversions._
import java.io._

class TopicJSFeaturizer extends AlignmentSimilarity {

  val NUM_TOPICS = 32

  var model: Option[LDATrainer] = None
  
  val MODEL_PATH = ParmaConfig.getFile("features.topicJS.lda.model")

  override def setup(docs: java.util.Collection[DocAlignment]) {

    if(MODEL_PATH.exists) {
      log("reading serialized lda model...")
      val ois = new ObjectInputStream(new FileInputStream(MODEL_PATH))
      model = Some(ois.readObject.asInstanceOf[LDATrainer])
      ois.close

      // Sanity checks
      if(model.get.model.num_topics != NUM_TOPICS) {
        log("mismatch in number of topics")
        sys.exit(1)
      }
    } else {
      log("estimating lda model...")

      // Get the corpus
      var lda_docs = scala.collection.mutable.ArrayBuffer.empty[edu.jhu.hlt.parma.inference.topics.Document]
      for(doc <- docs.flatMap(da => List(da.report, da.passage))) {
        val doc_string = doc.rawString
        lda_docs += edu.jhu.hlt.parma.inference.topics.Document.fromRawString(doc_string)
      }

      // Create the trainer, and run EM
      val trainer = new LDATrainer(NUM_TOPICS, edu.jhu.hlt.parma.inference.topics.Document.getVocabSize, lda_docs)
      trainer.runEM
      model = Some(trainer)

      // Serialize the trained model
      val oos = new ObjectOutputStream(new FileOutputStream(MODEL_PATH))
      oos.writeObject(trainer)
      oos.close
    }
  }

  private[this] val binarizer = new FixedWidthBinarizer(5, false, 0d, 1d)
  override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document)  {
    
    val report_string  = report.rawString
    val report_doc     = edu.jhu.hlt.parma.inference.topics.Document.fromRawString(report_string)

    val passage_string = passage.rawString
    val passage_doc    = edu.jhu.hlt.parma.inference.topics.Document.fromRawString(passage_string)

    val report_gamma  = Array.ofDim[Double](NUM_TOPICS)
    val passage_gamma = Array.ofDim[Double](NUM_TOPICS)
    
    val l1 = report_doc.length
    val l2 = passage_doc.length
    val lmax = if(l1>l2) l1 else l2

    val phi   = Array.ofDim[Double](lmax, NUM_TOPICS)

    model.get.inferencer.infer(report_doc, model.get.model, report_gamma, phi)
    model.get.inferencer.infer(passage_doc, model.get.model, passage_gamma, phi)

    val d = Stats.JSDistance( arrayToVector(report_gamma), arrayToVector(passage_gamma) )
	assert(d <= 1d, "go change binarizer: " + d)

  	featureIndexer.start(sv)
	featureIndexer.addStable("context-topic-jensen-shannon-distance", d, binarizer)
	featureIndexer.commit
  }

  def arrayToVector(a: Array[Double]) : SparseVector = {
    var v = new SparseVector(Int.MaxValue)
    for(i <- 0 until a.size) {
      v.set(i, a(i))
    }
    v
  }
}
