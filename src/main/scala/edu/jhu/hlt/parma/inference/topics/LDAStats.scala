// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference.topics

class LDAStats(numTopics : Int, vocabSize : Int) extends Serializable {

  var alpha_suffstats = 0.0
  val class_word      = Array.ofDim[Double](numTopics, vocabSize)
  val class_total     = Array.ofDim[Double](numTopics)
  var num_docs        = 0

}
