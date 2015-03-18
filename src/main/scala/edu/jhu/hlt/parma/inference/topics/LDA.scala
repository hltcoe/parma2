// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference.topics

class LDA(_numTopics : Int, _vocabSize : Int) extends Serializable {
  val num_topics = _numTopics
  val vocab_size = _vocabSize
  var alpha = 1.0
  val log_prob_w = Array.ofDim[Double](num_topics, vocab_size)
}

