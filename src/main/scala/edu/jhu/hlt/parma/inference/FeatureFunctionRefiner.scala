// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity

/**
 * NOTE: the strategy described below has largely been replaced by
 * the package feautures.generic, which allows you to build up
 * feature functions from small pieces rather than by splitting
 * large ones.
 * 
 * 
 * there are two options if you want to take a feature like
 * WordNet features and make them more "context aware"
 * 
 * 1. have a wrapper that decides if the features should be called
 *    at all, e.g. both report and passage mention's must be NPs
 *    or both be VPs (this does what we do now with MentionClassifierUtil)
 *    if this wrapper decides that things look funny, e.g. (NP,VP)
 *    mention pair, then it can shrink the features towards 0, or
 *    in the most extreme case make them 0 (hard cutoff)
 *    
 * 2. have a wrapper that forks the feature space according to the
 *    mentions given. this means you would have separate feature indices
 *    for (NP,NP) mention pairs as (VP,VP) mentions pairs (this does
 *    what we do now with extending e.g. AlignmentSimilarityFunction.CommonNounPair)
 *    
 * 3. related to #2, we may want to split features whether we are talking
 *    about an PredicateAlignment vs ArgCorefAlignment
 *    
 * think of this as the first hidden layer in a neural net
 */
class FeatureFunctionRefiner(
		val featureFunction: AlignmentSimilarity,
		val refiner: FeatureRefinement) extends AlignmentSimilarity {

	override def name: String = "%s-Refiner".format(featureFunction.name)
	override def setup(docs: java.util.Collection[DocAlignment]) { featureFunction.setup(docs) }
	override def cleanup { featureFunction.cleanup }

	private[this] val cardinality = refiner.numRefinements
	private[this] val svBuf = new SVec(120)
	//private[this] val alph = new Alphabet[String]

	override def setAlphabet(a: Alphabet[String]) {
		featureFunction.setAlphabet(a)
	}

	// NOTE: this implemenatation must match that of SVec.addWithStride
	override def featureName(idx: Int): String = {
		throw new RuntimeException("deprecated, use a global Alphabet instead")
		/*
		val nameIdx = idx % cardinality
		val innerIdx = idx / cardinality
		val name = alph.lookupObject(nameIdx)
		val innerName = featureFunction.featureName(innerIdx)
		name + innerName
		*/
	}

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		val alph = featureFunction.getAlphabet
		val refs = refiner.refine(a, report, passage)
		featureFunction.featurize(svBuf, a, report, passage)
		for((idx, value) <- svBuf.items) {
			val name = alph.lookupObject(idx)
			for((refName, refWeight) <- refs) {
				val fullName = refName + "." + name
				val fullIdx = alph.lookupIndex(fullName, addIfNotPresent=true)
				sv.add(fullIdx, value * refWeight)
			}
		}
		svBuf.clear
	}
}

trait FeatureRefinement extends Serializable {
	val name: String = getClass.getName.replace("edu.jhu.hlt.parma.", "")

	/**
	 * calls to refine should return a list no longer than this
	 */
	val numRefinements: Int

	/**
	 * calls to this should always return the same keys
	 */
	def refine(a: Alignment, report: Document, passage: Document): IndexedSeq[(String, Double)]
}

class AlignmentTypeRefinement extends FeatureRefinement {
	override val numRefinements = 2
	override def refine(a: Alignment, report: Document, passage: Document) = {
		IndexedSeq(
			("predicates-", if(a.isInstanceOf[PredicateAlignment]) 1d else 0d),
			("argcoref-", if(a.isInstanceOf[ArgCorefAlignment]) 1d else 0d)
		)
	}
}

class MentionClassifierRefinement extends FeatureRefinement {
	override val numRefinements = -1
	override def refine(a: Alignment, report: Document, passage: Document) = {
		/*
		val (reportCM, passageCM) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val reportT = MentionClassifierUtil.getMentionType(report, reportCM)
		val passageT = MentionClassifierUtil.getMentionType(passage, passageCM)
		val subspace = List(reportT.toString, passageT.toString).sorted.mkString(":")
		val coef = 1d
		Map(subspace -> coef)
		*/
		throw new RuntimeException("re-implement me")
	}
}

