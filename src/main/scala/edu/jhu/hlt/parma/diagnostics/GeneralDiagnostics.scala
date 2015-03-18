// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.diagnostics

import edu.jhu.hlt.parma.features.FeatureLoader
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.experiments._
import edu.jhu.hlt.parma.util._

object GeneralDiagnostics extends Logging2 {

	val PRED_OUTFILE = "diagnostics.predictions.all"
	
    def checkConfig {
		assert(ParmaConfig.getStrings(ParmaConfig.EXPERIMENTS).size > 0)
		//val nf = FeatureLoader.getFeatures.size
		//println("FeatureLoader yielded %d features".format(nf))
		//assert(nf > 0)
	}

	def checkCorpus(corpus: DocAlignmentCorpus[_<:DocAlignment]) {
		println("checking corpus...")
		if(corpus.train.size == 0)
			println("no training examples!")
		if(corpus.dev.size == 0)
			println("no dev examples!")
		if(corpus.test.size == 0)
			println("no test examples!")
		for(da <- corpus.trainAlignments.iterator ++ corpus.testAlignments.iterator) {
			// Roth and Frank data contains some empty alignments, which we should keep,
			// but in general the alignments should be none-empty
			// if they are empty there is most likely a bug to be found
			// dev/pair_6 is an exception (empty alignment)
			var exception = false
			exception |= da.id.contains("dev.pair6")	// no alignments
			exception |= da.id.contains("dev.pair4")	// only possible alignments
			if(da.possibleAlignments.size == 0 && !exception)
				warn(da.id + " has no alignments!")
			assert(da.possibleAlignments.size >= da.sureAlignments.size)
			checkDocAlignment(da)
		}
		println("corpus looks OK!")
	}

	def checkDocument(doc: Document) {
		// predicates and args should not have duplicates
		val pMentions = doc.predicates.map(_.location)
		assert(pMentions.size == pMentions.toSet.size, "all preds should be uniq by location in " + doc.id)
		val aMentions = doc.arguments.map(_.location)
		assert(aMentions.size == aMentions.toSet.size, "all args should be uniq by location in " + doc.id)
	}

	def checkDocAlignment(da: DocAlignment) {
		checkDocument(da.report)
		checkDocument(da.passage)
		for (pa <- da.possibleAlignments) {
			pa match {
				case p: PredicateAlignment => {
					checkMention(p.reportPred.location, da.report)
					checkMention(p.passagePred.location, da.passage)
				}
				case aa: ArgCorefAlignment => {
					for (a <- aa.reportCoref)
						checkMention(a.location, da.report)
					for (a <- aa.passageCoref)
						checkMention(a.location, da.passage)
				}
			}
		}
	}

	/**
	 * checks that this mention has valid indices in this doc
	 */
	def checkMention(mention: Mention, doc: Document) {
		assert(mention.getSentenceIdx >= 0)
		assert(mention.getSentenceIdx < doc.sentences.size)
		val s = doc.getSentence(mention)
		assert(mention.getStartTokenIdx >= 0)
		assert(mention.getEndTokenIdx <= s.tokens.size)
		assert(mention.getStartTokenIdx < mention.getEndTokenIdx)

		assert(mention.getStartTokenIdx < mention.getEndTokenIdx)
		assert(mention.getHeadTokenIdx >= mention.getStartTokenIdx)
		assert(mention.getHeadTokenIdx < mention.getEndTokenIdx)
	}

	def docAlignmentStatistics(alignments: Seq[DocAlignment], description: String): String = {
		val paa = (da: DocAlignment) => da.possibleAlignments.size
		val pa = (da: DocAlignment) => da.possiblePredicateAlignments.size
		val aa = (da: DocAlignment) => da.possibleArgCorefAlignments.size
		val sb = new StringBuilder
		val stats = (desc: String, alignments: Seq[DocAlignment]) => {
			sb.append("[corpusStatistics] in %s there are %d doc alignments\n".format(desc, alignments.size))
			sb.append("[corpusStatistics] in %s there are %d pred/arg alignments\n".format(desc, alignments.map(paa).sum))
			sb.append("[corpusStatistics] in %s there are %d predicate alignments\n".format(desc, alignments.map(pa).sum))
			sb.append("[corpusStatistics] in %s there are %d argCoref alignments\n".format(desc, alignments.map(aa).sum))
			sb.append("[corpusStatistics] in %s domains = %s\n".format(desc, alignments.groupBy(_.domain).mapValues(_.size)))
		}
		stats(description, alignments)
		sb.toString
	}
  
	def corpusStatistics(corpus: DocAlignmentCorpus[DocAlignment]): String = {
		CanonicalMentionDiagnostics.printCanonicalMentions(corpus)
		val sb = new StringBuilder
		sb.append(docAlignmentStatistics(corpus.trainAlignments, corpus.id + "_train"))
		sb.append(docAlignmentStatistics(corpus.devAlignments, corpus.id + "_dev"))
		sb.append(docAlignmentStatistics(corpus.testAlignments, corpus.id + "_test "))
		sb.toString
	}
	
	def outputPredictions(instances: Traversable[Instance[DocAlignment]]) {
		val filename = ParmaConfig.getString(PRED_OUTFILE, null)
		if(filename == null) return
		println("writing predictions")
		Profiler.startTask("outputPredictions")
		val out = new java.io.FileWriter(filename)   
		for(inst <- instances) {
			out.write(("="*40) + "\n")
			out.write(Describe.docAlignmentInstance(inst))
			out.write("\n\n")
		}
		out.close
		Profiler.endTask("outputPredictions")
  }
}


