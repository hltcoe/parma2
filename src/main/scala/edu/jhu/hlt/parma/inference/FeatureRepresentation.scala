// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.types._

/**
 * see InferenceEngine
 * 
 * this type is supposed to be the parent for any type
 * that is used to cache feature computation
 * 
 * an inference engine will produce a feature representation,
 * the computation for which will be front-loaded and cached,
 * and then asked to produce alignments from this representation
 */
trait FeatureRepresentation {
	def report: Document
	def passage: Document
	def context = new Context(report, passage)

	/**
	 * this is only used for debugging, not inference
	 * put in here a feature vector, indices should be
	 * look-up-able with controller.featureName(idx)
	 */
	def inspectFeatures: Option[scala.collection.Map[Alignment, SVec]]

	/**
	 * this is only used for debugging, not inference
	 * put in a probability or dot product in here
	 */
	def inspectScores: Option[scala.collection.Map[Alignment, Double]]

	/**
	 * give the InferenceEngine that made and uses this
	 * feature representation
	 */
	def controller: InferenceEngine[_]
}

