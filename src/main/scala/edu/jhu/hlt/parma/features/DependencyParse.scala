// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.features.framenet.FrameNet
import edu.jhu.hlt.parma.features.wordnet.WordNet
import edu.jhu.hlt.parma.util.Profiler
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import scala.collection.JavaConversions._

class DependencyParse extends AlignmentSimilarity {

	var wn: WordNet = null
	var fn: FrameNet = null
	
	override def setup(calibrateOn: java.util.Collection[DocAlignment]) {
		wn = WordNet.getInstance
		wn.setup
		fn = FrameNet.getInstance
	}
	
	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
	
		featureIndexer.start(sv)

		// TODO add or experiment with edge types for dependencies
		// does adj("foo", "bar") differ significantly from prep("foo", "bar")?
		
		val maxDist = 2

		// TODO add count binarizer
		
		val wordSim = (w1: String, w2: String) => {
			val sd: Int = Profiler.time("wordSim-1", Unit => wn.getSynonymDistance(w1, w2, maxDist))
			val md: Int = Profiler.time("wordSim-2", Unit => math.max(wn.getHypernymDistance(w1, w2, maxDist), wn.getHypernymDistance(w2, w1, maxDist)))
			val cd: Int = Profiler.time("wordSim-3", Unit => math.max(fn.getChildDistance(w1, w2, maxDist), fn.getChildDistance(w2, w1, maxDist)))
			val pd: Int = Profiler.time("wordSim-4", Unit => math.max(fn.getPerspectiveChildDistance(w1, w2, maxDist), fn.getPerspectiveChildDistance(w2, w1, maxDist)))
			(sd, md, cd, pd)
		}
		
		// union over all things reportCM governs, see how many are similar
		val (rGoverns, pGoverns) = Profiler.time("union-gov", Unit => a.unionGoverns(report, passage))
		val govPairs = Profiler.time("union-gov-crossprod", Unit => for(rg <- rGoverns; pg <- pGoverns) yield (rg, pg))
		val gSynHyp = Profiler.time("union-gov-map", Unit => govPairs.map(rp => wordSim(rp._1.dep.getWord, rp._2.dep.getWord)))
		if(gSynHyp.size > 0) {
			featureIndexer.addUnstable("gov-wn-syn-min" + gSynHyp.map(_._1).min.toString, bool2value(true))
			featureIndexer.addUnstable("gov-wn-hyp-min" + gSynHyp.map(_._2).min.toString, bool2value(true))
			featureIndexer.addUnstable("gov-fn-child-min" + gSynHyp.map(_._3).min.toString, bool2value(true))
			featureIndexer.addUnstable("gov-fn-perspChild-min" + gSynHyp.map(_._4).min.toString, bool2value(true))
		}
		else featureIndexer.addUnstable("gov-empty", bool2value(true))

		// same for dependents
		val (rDepends, pDepends) = Profiler.time("union-dep", Unit => a.unionGovernedBy(report, passage))
		val depPairs = Profiler.time("union-dep-crossprod", Unit => for(rd <- rDepends; pd <- pDepends) yield (rd, pd))
		val dSynHyp = Profiler.time("union-dep-map", Unit => depPairs.map(rp => wordSim(rp._1.gov.getWord, rp._2.gov.getWord)))
		if(dSynHyp.size > 0) {
			featureIndexer.addUnstable("dep-syn-wn-min" + dSynHyp.map(_._1).min.toString, bool2value(true))
			featureIndexer.addUnstable("dep-hyp-wn-min" + dSynHyp.map(_._2).min.toString, bool2value(true))
			featureIndexer.addUnstable("dep-fn-child-min" + dSynHyp.map(_._3).min.toString, bool2value(true))
			featureIndexer.addUnstable("dep-fn-perspChild-min" + dSynHyp.map(_._4).min.toString, bool2value(true))
		}
		else featureIndexer.addUnstable("dep-empty", bool2value(true))

		featureIndexer.commit
	}
	
}


