// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.experiments.Corpus
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.math.Stats
import edu.jhu.hlt.phylo.util.UnigramLM
import no.uib.cipr.matrix.VectorEntry
import no.uib.cipr.matrix.sparse.SparseVector
import scala.util.control.Breaks._
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
  
class ELMContextSimilarityFeature extends AlignmentSimilarity {
  
  var priorWt = 0.25
  var priorLM = Array.empty[Double]

  var tokenMap = new HashMap[String, Integer]()

  val fast = true	// optimizations, talk to travis if curious

  // This will estimate the backoff/prior LM weights by iterating
  // through the entire corpus.
  override def setup(das: java.util.Collection[DocAlignment]) {
  	val docs = das.flatMap(da => List(da.report, da.passage))

    var corpusLM = new UnigramLM[Integer]()

    // Compute the prior LM weights by making a pass through the corpus
	if(fast) {
		docs.foreach(d =>
			d.allTokens.map(_.getWord).foreach(t =>
				corpusLM.incrementCount(tokenMap.getOrElseUpdate(t, tokenMap.size))
			)
		)
	} else {
		for(doc <- docs) {
		  for(token <- doc.allTokens.map(_.getWord)) {
			if(!tokenMap.containsKey(token)) {
			  tokenMap.put( token, tokenMap.size )
			}
			val index = tokenMap(token)
			corpusLM.incrementCount( index )
		  }
		}
	}

    corpusLM.smoothAndNormalize()
    
    println("vocab size = " + corpusLM.vocabSize())
    priorLM = new Array[Double](corpusLM.vocabSize())
	if(fast) {
		var i = 0
		val n = corpusLM.vocabSize
		while(i < n) {
		  priorLM(i) = corpusLM.getProb(i)
		  i += 1
		}
	} else {
		for(i <- 0 until corpusLM.vocabSize()) {
		  val p = corpusLM.getProb(i)
		  priorLM(i) = p
		}
	}
  }

  private[this] val binarizer = new FixedWidthBinarizer(5, false, 0d, 1d)
  override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
    
    var context1 = getNormalizedMeasure(getMentionContextVector(report))
    var context2 = getNormalizedMeasure(getMentionContextVector(passage))
    
    val d = Stats.JSDistance(context1, context2)
	assert(d <= 1 && d >= 0, "fix binarizer: "+ d)
    
	featureIndexer.start(sv)
    featureIndexer.addStable("context-lm-jensen-shannon-distance", d, binarizer)
	featureIndexer.commit
  }
  
  def getMentionContextVector(doc: Document) : SparseVector = {
    var tokens = doc.allTokens.map(_.getWord)
    var v = new SparseVector(Int.MaxValue)

	if(fast) {
		tokens.foreach(t => v.add(tokenMap(t), 1.0))	// TODO this sparse vector is garbage! optimize this
	} else {
		for(t <- tokens) {
		  val index = tokenMap(t)
		  assert(index >= 0)
		  v.add(index, 1.0)
		}
	}

    return v
  }

  def getNormalizedMeasure(sv: SparseVector) : SparseVector = {
//    var cDist = new SparseVector(Integer.MAX_VALUE)
    var cDist = new SparseVector(Int.MaxValue)
    cDist.add(sv)
    normalize(cDist)
    return cDist
  }

  def normalize(sv: SparseVector) {
    val N = Stats.vsum(sv)
    for(ve <- sv) {
      // Bayesian estimate per Manning (2008)
        var p_t_e = (ve.get() + (priorWt * priorLM(ve.index()))) / (N + priorWt)
      
        if(p_t_e == 0.0)
	      throw new RuntimeException(String.format("%s: Underflow detected! %s",this.getClass().getName(),ve.toString()));
      sv.set(ve.index(), p_t_e)
    }
  }
}
