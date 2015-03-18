// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.generic

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.{ CanonicalMentionFinder, DocMetaAligner }
import edu.jhu.hlt.parma.feature_interfaces._
import collection.JavaConversions._
import collection.mutable.ArrayBuffer
import util.Random

class ConciseFeaturesSlow extends ConciseFeatures {
	override val quickly = false
}

class ConciseFeatures extends AlignmentSimilarity {

	import ExtractorImplementation._
	import TransformImplementation._
	import SimilarityImplementation._
	import AccumulatorImplementation._
	import GeneralPipeImplementations._

	val quickly = true
	val caseSensitive = false
	
	// TODO need to add a callback to flush these
	val flushEveryAlignment = new ArrayBuffer[CachingPipe[_,_]]

	// feature functions
	var ff = new ArrayBuffer[Pipe[(Context, Alignment), DVec]]

	val defaultLexicalSim = IndexedSeq(stringEq, wordNet, frameNet, nameTransducer, jaroWinklerDistance)
	val cheapLexicalSim = IndexedSeq(stringEq, nameTransducer, jaroWinklerDistance)
	val allLexicalSim = (defaultLexicalSim ++ cheapLexicalSim).toSet.toIndexedSeq
	
	// [*] canonical mention, head-to-head features
	for(sim <- cheapLexicalSim)
		ff += (canonicalMention >>= headToken >>= word >>= sim >>= IdentAccum)
	for(sim <- defaultLexicalSim) {
		ff += (canonicalMention >>= headToken >>= lemma >>= lowercase >>= sim >>= IdentAccum)
		ff += (canonicalMention >>= headToken >>= lemma >>= sim >>= IdentAccum)
	}

	// [*] all tokens in the head mention
	for(sim <- allLexicalSim)	{
		ff += (canonicalMention >>= allTokens >>= word.forSets >>= lowercase.forSets >>= jaccard >>= IdentAccum)
		ff += (canonicalMention >>= allTokens >>= word.forSets >>= jaccard >>= IdentAccum)
		ff += (canonicalMention >>= allTokens >>= lemma.forSets >>= jaccard >>= IdentAccum)
		ff += (canonicalMention >>= allTokens >>= pos.forSets >>= jaccard >>= IdentAccum)
		ff += (canonicalMention >>= allTokens >>= ner.forSets >>= jaccard >>= IdentAccum)
	}

	// [*] all mentions' heads
	for(sim <- defaultLexicalSim) {
		ff += (allMentions >>= headToken >>= lemma >>= lowercase >>= sim.pavg)
		ff += (allMentions >>= headToken >>= lemma >>= lowercase >>= sim.pmax)
		ff += (allMentions >>= headToken >>= lemma >>= lowercase >>= sim.pmin)
	}
	for(sim <- cheapLexicalSim) {
		ff += (allMentions >>= headToken >>= word >>= lowercase >>= sim.pavg)
		ff += (allMentions >>= headToken >>= word >>= lowercase >>= sim.pmax)
		ff += (allMentions >>= headToken >>= word >>= lowercase >>= sim.pmin)
		if(caseSensitive) {
			ff += (allMentions >>= headToken >>= word >>= sim.pavg)
			ff += (allMentions >>= headToken >>= word >>= sim.pmax)
			ff += (allMentions >>= headToken >>= word >>= sim.pmin)
		}
	}

	// [*] disjucntion features: stringSim < eps || (x,y) in wordNet
	for(t <- 0.1d to 2.2d by 0.15d)
		ff += (canonicalMention >>= headToken >>= lemma >>= wordNetAndNameTransducer(t) >>= IdentAccum)
	ff += (canonicalMention >>= headToken >>= lemma >>= wordNetAndStringEq >>= IdentAccum)
	for(t <- 0.1d to math.log(2d) by 0.25d)
		ff += (canonicalMention >>= headToken >>= lemma >>= wordNetAndJaroWinkler(t) >>= IdentAccum)


	// crude morphology
	if(!quickly) {
		for(affix <- IndexedSeq(prefix _, suffix _); i <- 1 to 5)
			ff += (canonicalMention >>= headToken >>= word >>= lowercase >>= affix(i) >>= stringEq >>= IdentAccumMaybeEmpty(stringEq.outputDimension))
		
		// words in the mention which are not the head
		for(n <- 2 to 4) {
			ff += (canonicalMention >>= beforeHeadString >>= lowercase >>= ngrams(n) >>= jaccard >>= IdentAccumMaybeEmpty(jaccard.outputDimension))
			ff += (canonicalMention >>= afterHeadString >>= lowercase >>= ngrams(n) >>= jaccard >>= IdentAccumMaybeEmpty(jaccard.outputDimension))
			ff += (canonicalMention >>= fullMentionString >>= lowercase >>= ngrams(n) >>= jaccard >>= IdentAccum)
		}

		// all mentions, n-grams
		for(n <- 2 to 4) {
			ff += (allMentions >>= fullMentionString >>= lowercase >>= ngrams(n) >>= jaccard.pavg)
			ff += (allMentions >>= fullMentionString >>= lowercase >>= ngrams(n) >>= jaccard.pmax)
			ff += (allMentions >>= fullMentionString >>= lowercase >>= ngrams(n) >>= jaccard.pmin)
			ff += (allMentions >>= headToken >>= word >>= lowercase >>= ngrams(n) >>= jaccard.pavg)
			ff += (allMentions >>= headToken >>= word >>= lowercase >>= ngrams(n) >>= jaccard.pmax)
			ff += (allMentions >>= headToken >>= word >>= lowercase >>= ngrams(n) >>= jaccard.pmin)
		}

		// [*] dependency parse features
		val depOnCM_allPairs = (canonicalMention >>= dependentTokens >>= allPairs[Token]).withCaching
		val govCM_allPairs = (canonicalMention >>= governingTokens >>= allPairs[Token]).withCaching
		//val flushEveryAlignment = Seq(depOnCM_allPairs, govCM_allPairs)
		flushEveryAlignment ++= Seq(depOnCM_allPairs, govCM_allPairs)
		for(sim <- defaultLexicalSim) {
			ff += (depOnCM_allPairs >>= lemma >>= lowercase >>= sim.pavg)
			ff += (depOnCM_allPairs >>= lemma >>= lowercase >>= sim.pmax)
			ff += (depOnCM_allPairs >>= lemma >>= lowercase >>= sim.pmin)
			ff += (govCM_allPairs >>= lemma >>= lowercase >>= sim.pavg)
			ff += (govCM_allPairs >>= lemma >>= lowercase >>= sim.pmax)
			ff += (govCM_allPairs >>= lemma >>= lowercase >>= sim.pmin)
		}
		for(sim <- cheapLexicalSim) {
			ff += (depOnCM_allPairs >>= word >>= lowercase >>= sim.pavg)
			ff += (depOnCM_allPairs >>= word >>= lowercase >>= sim.pmax)
			ff += (depOnCM_allPairs >>= word >>= lowercase >>= sim.pmin)
			ff += (govCM_allPairs >>= word >>= lowercase >>= sim.pavg)
			ff += (govCM_allPairs >>= word >>= lowercase >>= sim.pmax)
			ff += (govCM_allPairs >>= word >>= lowercase >>= sim.pmin)
			if(caseSensitive) {
				ff += (depOnCM_allPairs >>= word >>= sim.pavg)
				ff += (depOnCM_allPairs >>= word >>= sim.pmax)
				ff += (depOnCM_allPairs >>= word >>= sim.pmin)
				ff += (govCM_allPairs >>= word >>= sim.pavg)
				ff += (govCM_allPairs >>= word >>= sim.pmax)
				ff += (govCM_allPairs >>= word >>= sim.pmin)
			}
		}
		
		
		// [*] sentence context features
		for(trans <- IndexedSeq(word, lemma, pos, ner, normNer)) {
			ff += (sentenceTokens >>= trans.forSets >>= lowercase.forSets >>= jaccard >>= IdentAccum)
			ff += (sentenceTokensAfterCM >>= trans.forSets >>= lowercase.forSets >>= jaccard >>= IdentAccum)
			ff += (sentenceTokensBeforeCM >>= trans.forSets >>= lowercase.forSets >>= jaccard >>= IdentAccum)
		}


		// [*] path features
		// TODO make an implicit conversion for when you leave off IdentAccum
		ff += (rootToCM >>= treeToValue.forSeqs >>= levenshtein >>= IdentAccum)
		
		ff += (rootToCM >>= treeToValue.forSeqs >>= joinStr >>= jaroWinklerDistance >>= IdentAccum)
		ff += (rootToCM >>= treeToValue.forSeqs >>= joinStr >>= stringEq >>= IdentAccum)
		

		// [*] misc features
		ff += (asymmetricHeadGovDep >>= lemma.forSets >>= lowercase.forSets >>= intersect.pmax)
		ff += (asymmetricHeadGovDep >>= word.forSets >>= lowercase.forSets >>= intersect.pmax)
		ff += (asymmetricHeadGovDep >>= pos.forSets >>= intersect.pmax)
		ff += (asymmetricHeadGovDep >>= ner.forSets >>= intersect.pmax)
		ff += (asymmetricHeadGovDep >>= normNer.forSets >>= intersect.pmax)
		if(caseSensitive) {
			ff += (asymmetricHeadGovDep >>= lemma.forSets >>= intersect.pmax)
			ff += (asymmetricHeadGovDep >>= word.forSets >>= intersect.pmax)
		}

		val special = IndexedSeq("and", "or", "of", "a", "the", "what", "who", "where", "when",
			"which", "after", "before", "during", "near", "his", "her", "him", "he", "she", "it")
		for(specialWord <- special)
			ff += (canonicalMention >>= allTokens >>= word.forSets >>= lowercase.forSets >>= bothContain(specialWord) >>= IdentAccum)
		ff += (canonicalMention >>= allTokens >>= pos.forSets >>= bothContain("CC") >>= IdentAccum)	
		
		// consider tense/aspect modifiers
		// TODO do not use this for now, see details in onlyPOS
		//ff += (canonicalMention >>= allTokens >>= onlyPOS("AUX", prefix=true).forSets >>= lemma.forSets >>= jaccard >>= IdentAccum)
	}	
	
	
	override def setup(calibrateOn: java.util.Collection[DocAlignment]) = {
		SimilarityImplementation.setup
		import edu.jhu.hlt.parma.util.Reservoir.Sample
		import collection.JavaConversions._
		implicit val rand = new Random(9001)
		val start = System.currentTimeMillis
		var normalized = ff.map(x => 0).toBuffer
		val howMany = 25
		log("[ConciseFeatures setup] normalizing features on %d doc alignments".format(howMany))
		for(da <- calibrateOn.toSeq.reservoir(howMany)) {
			for(a <- DocMetaAligner.allPossibleAlignments(da.context)) {
				ff = ff.zipWithIndex.map(fi => {
					val (f, fIdx): (Pipe[(Context, Alignment), DVec], Int) = fi
					val features = f(da.context, a)
					val m = features.lInf
					if(m > 1.5d) {
						normalized(fIdx) = 1
						f >>= (new Pipe[DVec, DVec]("scale_down", (d: DVec) => d * (1/m)))
					}
					else f
				})
			}
		}
		log("[ConciseFeatures setup] normalized %d features in %.1f sec"
			.format(normalized.sum, (System.currentTimeMillis-start)/1000d))
	}

	// TODO remove linear search
	// low priority because this doesn't happen in the feature computation loop
	override def featureName(idx: Int): String = {
		val debug = false
		if(ffWidths.size == 0)
			throw new RuntimeException("you can't call this until i've called the features!")
		var curIdx = 0
		for((width, i) <- ffWidths.zipWithIndex) {
			if(debug) println("idx=%d i=%d width=%d curIdx=%d feature=%s".format(idx, i, width, curIdx, ff(i).name))
			if(idx < curIdx + width)
				return ff(i).featureName(idx - curIdx)
			//curIdx += width
			curIdx = curIdx + width
		}
		warnIf(debug, "could not find: idx=%d, ffWidths.sum=%d, ffWidths.size=%d, ffWidths=%s".format(idx, ffWidths.sum, ffWidths.size, ffWidths))
		null
	}
	
	private[this] var callsToFeaturize = 0
	private[this] val ffWidths = new ArrayBuffer[Int]

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		val debug = false

		//println("[concise] before callseToFeaturize = " + callsToFeaturize)
		//callsToFeaturize += 1
		callsToFeaturize = callsToFeaturize + 1
		//println("[concise] after callseToFeaturize = " + callsToFeaturize)
		val context = new Context(report, passage)
		var offset = 0
	
		if(debug) println("[concise featurize] start sv = " + Describe.svec(sv))

		for((f, fIdx) <- ff.zipWithIndex) {

			//if(debug) println("[concise] calling(%d) features(%d) = %s".format(callsToFeaturize, fIdx, f.name))
			//val values = Profiler.time("CF:" + f.name, Unit => f(context, a))
			val values = f(context, a)

			// check that the width of the returned features has not changed
			if(callsToFeaturize == 1) {
				if(debug) println("[concise] adding width(%d) = %d".format(fIdx, values.dimension))
				ffWidths += values.dimension
			}
			else {
				assert(ffWidths.size == ff.size, "ffWidths.size=%d ff.size=%d".format(ffWidths.size, ff.size))
				assert(ffWidths(fIdx) == values.dimension,
					"feature=%s prevWidth=%d curWidth=%d".format(f.name, ffWidths(fIdx), values.dimension))
			}

			sv.add(values, offset)
			//offset += values.dimension
			offset = offset + values.dimension
			//if(debug) println("after %s, offset=%d, values.dim=%d, values=%s, sv=%s".format(f.name, offset, values.dimension, values, Describe.svec(sv)))
			//values.foreach(vi =>
			//	featureIndexer.addStable(f.name + "-" + vi._1, vi._2))
		}
		assert(ffWidths.size > 0)

		// the main reason I added caching was for all-pairs stuff
		// for just one alignment. If later you think you need to keep
		// it for other stuff, then remove this, but otherwise i will
		// flush after every alignment just to keep memory in check
		flushEveryAlignment.foreach(cp => {
			//println("\tclearing " + cp)
			//println("\tafter " + Describe.alignment(a, report, passage))
			cp.clear
		})
		if(debug) println("[concise featurize] end sv = " + Describe.svec(sv))
	}
}



