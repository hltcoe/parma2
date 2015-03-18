// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.features._
import edu.jhu.hlt.parma.diagnostics.BadAlignmentDiagnostics
import util.Random

/**
 * provides bunch of stuff that Pipeline uses
 */
trait Experiment[T <: InferenceEngine[_]] {

	def name: String = this.getClass.getName.replace("edu.jhu.hlt.parma.experiments.", "")

	/**
	 * where to do logging and put output
	 */
	def workingDirectory: Option[WorkingDirectory] = None
	
	/**
	 * gets the data for the experiment
	 */
	def rawData: Corpus[DocAlignment]
	
	/**
	 * how should I produce alignments?
	 */
	def inferenceEngine: T
	
 	/**
	 * what should we optimize?
	 * defaults to F1
	 */
	def loss: EvaluationLoss = MacroF1.asLossFunction
	
	/**
	 * return a list of evaluation metrics to run
	 * return type should be a name and a number
	 * 
	 * defaults are pretty exhaustive, but you can override
	 * this method by adding functions to it
	 */
	def evaluationFunctions: Seq[EvaluationFunction] = EvaluationFunction.allFunctions
	
	/**
	 * provide the train-dev-test splits that you want the pipeline to execute
	 * 
	 * assume that there is a 1-1 between Ts and DAs
	 * define a partition based on DAs, map it back to Ts
	 * (T will typically be a feature representation)
	 */
	def evaluationSplits[F](c: Corpus[F])(implicit asDocAlignment: F => DocAlignment): Seq[Corpus[F]] =
		EvaluationSplits.asIs(c)
	
}

