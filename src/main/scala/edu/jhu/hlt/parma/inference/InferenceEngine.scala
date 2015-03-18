// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.evaluation.SetBasedEvaluator
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import java.io.File

trait InferenceEngine[F <: FeatureRepresentation] extends Logging2 {

	def name: String = getClass.toString.replace("edu.jhu.hlt.parma.inference.", "")

	/**
	 * though inference is usually done through align and other methods,
	 * this method is useful for debugging
	 * @see featureName for feature names
	 */
	def parameters: DVec

	/**
	 * returns the (name, index, weight) of the k parameters that
	 * have the greatest magnitude (absolute value)
	 */
	def biggestParameters(k: Int = 10): Seq[(String, Int, Double)] = {
		val p = parameters	// might make a copy
		for(idx <- (0 until p.dimension).toBuffer[Int].sortBy(i => -math.abs(p(i))).take(k) if p(idx) != 0d)
			yield (featureName(idx), idx, p(idx))
	}

	def logBiggestParameters(k: Int = 10) {
		for((name, index, weight) <- biggestParameters(k))
			log("[logBiggestParameters] %s:%d => %.2f".format(name, index, weight))
	}


	/**
	 * used to check if weights are what you think they should be
	 * @see parameters for matching indices
	 */
	def featureName(index: Int): String

	/**
	 * you're feature representation should be fixed after
	 * this call for to allow the pipeline to coherently cache
	 * features. if you do not wish to abide by this your only
	 * (well-defined) recourse is to not return feature representations
	 */
	def preTrainCalibrate(examples: Seq[DocAlignment])
	
	
	def train(examples: Seq[DocAlignment]): Option[Seq[DocAlignmentWithFeatures[F]]] = {
		val dawf = examples.map(da => computeFeatures(da))
		train(dawf)
		Some(dawf)
	}
	def train(examples: Seq[DocAlignmentWithFeatures[F]])	// used for CV + precomputed features
	
	/**
	 * used for tuning parameters on a dev set
	 * NOTE: make sure you are providing a LOSS FUNCTION to be minimize (not an objective function to be maximized)
	 */
	def postTrainCalibrate(examples: Seq[DocAlignment], loss: EvaluationLoss):
			Option[Seq[DocAlignmentWithFeatures[F]]] = {
		val dawf = examples.map(da => computeFeatures(da))
		postTrainCalibrate(dawf, loss)
		Some(dawf)
	}
	// used for CV + precomputed features
	/*
	 * NOTE: make sure you are providing a LOSS FUNCTION to be minimize (not an objective function)
	 */
	def postTrainCalibrate(examples: Seq[DocAlignmentWithFeatures[F]], loss: EvaluationLoss)

	
	/**
	 * override this implementation to get feature caching in the pipeline
	 */
	def computeFeatures(da: DocAlignment): DocAlignmentWithFeatures[F] =
		new DocAlignmentWithFeatures(da, computeFeatures(da.report, da.passage, da.domain))

	// this implementation is essential for making predictions
	def computeFeatures(report: Document, passage: Document, domain: Option[String]): F
	
	
	/**
	 * @deprecated used only for legacy reasons
	 * DO NOT OVERRIDE/IMPLEMENT
	 */
	def align(fr: F): DocAlignment = align(Seq(fr)).head

	/**
	 * @deprecated used only for legacy reasons
	 * DO NOT OVERRIDE/IMPLEMENT
	 */
	def align(report: Document, passage: Document, domain: Option[String]): DocAlignment =
		align(Seq(report), Seq(passage), Seq(domain)).head
		

	/**
	 * override this implementation to get feature caching in the pipeline
	 */
	def align(frs: Seq[F]): Seq[DocAlignment]

	def align(reports: Seq[Document], passages: Seq[Document], domains: Seq[Option[String]]): Seq[DocAlignment] = {
		align(
			for(((r,p),d) <- reports.zip(passages).zip(domains))
				yield computeFeatures(r,p,d)
		)
	}

	/**
	 * return a score for a doc alignment
	 * this might be a log-likelihood, a margin, etc
	 *
	 * by default it just aligns the docs in the given alignment
	 * and returns the F1 between those alignments
	 */
	def score(da: DocAlignment, domain: Option[String]): Double = {
		val predictedDA = align(da.report, da.passage, domain)
		SetBasedEvaluator.generousF1(new Instance(da, predictedDA))	// hyp=da, gold=predictedDA => only matters for prec/rec
	}


	/**
	 * meant to be just human readable output for now
	 */
	def writeoutParameters(f: File)
	def writeoutParameters(id: String) {
		val parent = ParmaConfig.getDirectory(ParmaConfig.PARAMETER_OUTPUT_FILE)
		val f = new File(parent, id)
		if(!parent.exists)
			warn("not writing out parameters for %s because %s does not exist!".format(this, parent.getPath))
		else {
			log("[writeoutParameters] %s is writing parameters to %s".format(name, f.getPath))
			writeoutParameters(f)
		}
	}

	def readParameters(f: File)
	def readParameters(id: String) {
		val parent = ParmaConfig.getDirectory(ParmaConfig.PARAMETER_OUTPUT_FILE)
		val f = new File(parent, id)
		log("[readParameters] %s is reading parameters from %s".format(name, f.getPath))
		readParameters(f)
	}

}


