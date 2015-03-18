// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import java.util.logging._

class PositionalFeatures extends AlignmentSimilarity {

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		
		featureIndexer.start(sv)

		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rs: Int = reportMention.getSentenceIdx
		val ps: Int = passageMention.getSentenceIdx
		featureIndexer.addStable("same-sentence", bool2value(rs == ps))
		featureIndexer.addStable("sent-diff", math.abs(rs - ps)/2d)
		featureIndexer.addStable("min-sentence", math.min(rs, ps))
		featureIndexer.addStable("max-sentence", math.max(rs, ps) / 50d)

		val rN: Int = report.sentences.size
		val pN: Int = passage.sentences.size
		featureIndexer.addStable("same-doc-half", bool2value((2*rs)/rN == (2*ps)/pN))
		featureIndexer.addStable("same-doc-quarter", bool2value((4*rs)/rN == (4*ps)/pN))

		val isHead = (m: Mention) => (m.getHeadTokenIdx == m.getStartTokenIdx)
		featureIndexer.addStable("both-isHead", bool2value(isHead(reportMention) && isHead(passageMention)))
		featureIndexer.addStable("exactly-one-isHead", bool2value(isHead(reportMention) ^ isHead(passageMention)))

		val rn = reportMention.getEndTokenIdx - reportMention.getStartTokenIdx
		val pn = passageMention.getEndTokenIdx - passageMention.getStartTokenIdx
		featureIndexer.addStable("same-numWords", bool2value(rn == pn))

		featureIndexer.commit
	}
}

