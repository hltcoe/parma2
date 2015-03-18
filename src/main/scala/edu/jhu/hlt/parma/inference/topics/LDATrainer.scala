// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference.topics

import org.apache.commons.math3.special.Gamma
import scala.collection.mutable.ArrayBuffer
import scala.math._

class LDATrainer(numTopics: Int, vocabSize: Int, @transient docs: ArrayBuffer[Document]) extends Serializable {

  // Trainer settings
  val NUM_INIT = 1
  var VAR_MAX_ITER = 20 // If EM fails, this will be increased.
  val VAR_CONVERGED = 1e-6
  var EM_MAX_ITER = 100
  val EM_CONVERGED = 1e-4
  val ESTIMATE_ALPHA = false

  // val rnd = new scala.util.Random(12345)
  // val docs = corpus
  val model = new LDA(numTopics, vocabSize)
  val inferencer = new LDAInferencer(VAR_CONVERGED, VAR_MAX_ITER)
  val stats = new LDAStats(numTopics, vocabSize)

  def maxCorpusLength(docs: ArrayBuffer[Document]) : Int = {
    var max = 0
    for(doc <- docs) {
      if(doc.length > max) max = doc.length
    }
    max
  }

  def setMaxEMIter(iter: Int) {
    EM_MAX_ITER = iter
  }

  def zeroInitialize {
    var k = 0
    var w = 0
    while(k < model.num_topics) {
      stats.class_total(k) = 0.0
      w = 0
      while(w < vocabSize) {
        stats.class_word(k)(w) = 0.0
        w += 1
      }
      k += 1
    }
  }

  // Infer a particular document's 
  def getDocumentTopicDist(doc: Document) : Array[Double] = {
    val gamma  = Array.ofDim[Double](numTopics)
    val l = doc.length
    val phi   = Array.ofDim[Double](l, numTopics)
    inferencer.infer(doc, model, gamma, phi)
    gamma
  }

  def getTopicWordProb(topic: Int, word: Int) : Double = {
    stats.class_word(topic)(word)
  }

  // Java <=> Scala
  def getNumTopics : Int = {
    numTopics
  }

  def runEM {

    // 1. Initialize variational parameters
    val var_gamma = Array.ofDim[Double](docs.size, model.num_topics)
    val max_length = maxCorpusLength(docs)

    println("max length = " + max_length)

    val phi = Array.ofDim[Double](max_length, model.num_topics)

    // 2. Initialize the model
    init
    maximize

    println("model alpha = " + model.alpha)

    // 3. Run EM
    var iter = 0
    var likelihood = 0.0
    var likelihood_old = 0.0
    var converged = 1.0
    while (((converged < 0) || (converged > EM_CONVERGED) || (iter <= 2)) && (iter <= EM_MAX_ITER)) {
      iter += 1
      println("**** em iteration " + iter + " ****\n")
      likelihood = 0.0

      zeroInitialize

      // E-Step
      println("e-step...")
      var d = 0
      while(d < docs.size) {
        likelihood += docEStep(docs(d), var_gamma(d), phi)
        d += 1
      }

      println("likelihood: " + likelihood)

      // M-Step
      println("m-step...")
      maximize

      // Check for convergence
      converged = (likelihood_old - likelihood) / (likelihood_old)
      if(converged < 0) VAR_MAX_ITER = VAR_MAX_ITER * 2
      likelihood_old = likelihood
    }
  }

  def init {
    var k = 0
    var i = 0
    var n = 0
    while(k < model.num_topics) {
      i = 0
      while(i < NUM_INIT) {
        // val d = floor(rnd.nextDouble * docs.size)
        val d = k
        //println("initialized with document " + d)
        val doc = docs(d)
        n = 0
        while(n < doc.length) {
          stats.class_word(k)(doc.words(n)) += doc.counts(n)
          n += 1
        }
        i += 1
      }
      n = 0
      while(n < vocabSize) {
        stats.class_word(k)(n) += 1
        stats.class_total(k)   += stats.class_word(k)(n)
        n += 1
      }
      k += 1
    }

    // XXX DEBUG print class total
    // k = 0
    // while(k < model.num_topics) {
    //   println("class_total["+k+"]="+stats.class_total(k))
    //   k += 1
    // }
  }

  def docEStep(doc: Document, gamma: Array[Double], phi: Array[Array[Double]]) : Double = {

    // Posterior inference
    val likelihood = inferencer.infer(doc, model, gamma, phi)

    // Update sufficient statistics
    var gamma_sum = 0.0
    var k = 0
    while(k < model.num_topics) {
      gamma_sum += gamma(k)
      stats.alpha_suffstats += Gamma.digamma(gamma(k))
      k += 1
    }
    stats.alpha_suffstats -= model.num_topics * Gamma.digamma(gamma_sum)

    var n = 0
    while(n < doc.length) {
      var k = 0
      while(k < model.num_topics) {
        stats.class_word(k)(doc.words(n)) += doc.counts(n) * phi(n)(k)
        stats.class_total(k)              += doc.counts(n) * phi(n)(k)
        k += 1
      }
      n += 1
    }

    stats.num_docs += 1

    likelihood
  }

  // Compute MLE LDA model from sufficient stats
  def maximize {
    var k = 0
    while(k < model.num_topics) {
      var w = 0
      while(w < model.vocab_size) {
        if(stats.class_word(k)(w) > 0) {
          model.log_prob_w(k)(w) = Math.log(stats.class_word(k)(w)) -
            Math.log(stats.class_total(k));
        } else {
          model.log_prob_w(k)(w) = -100
        }
        w += 1
      }
      k += 1
    }
    if(ESTIMATE_ALPHA) {
      // TODO
    }
  }
}
